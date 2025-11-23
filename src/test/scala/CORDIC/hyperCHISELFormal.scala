package CORDIC

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._

class HyperCordicFormal(val width: Int, val cycleCount: Int, val integerBits: Int = 3, val magnitudeCorrection: Boolean = true) extends Module {
  import HyperCordicConstants.Mode
  
  // Parameter Validations
  require(width > 0, "Width must be positive")
  require(cycleCount > 0, "Cycle count must be positive")
  require(integerBits >= 1, "Integer bits must be at least 1 for the sign bit.")
  val fractionalBits: Int = width - 1 - integerBits
  require(fractionalBits > 0, s"Fractional bits must be positive. Check width ($width) vs integerBits ($integerBits).")

  val io = IO(new Bundle {
    // Control
    val start = Input(Bool())
    val mode = Input(Mode())

    // Data Inputs
    val targetTheta = Input(SInt(width.W))
    val inputX = Input(SInt(width.W))
    val inputY = Input(SInt(width.W))

    // Data Outputs
    val done = Output(Bool())
    val coshOut = Output(SInt(width.W))
    val sinhOut = Output(SInt(width.W))
    val atanhOut = Output(SInt(width.W))
    val magnitudeResultHyper = Output(SInt(width.W))
    val expOut = Output(SInt(width.W))
    val expNegOut = Output(SInt(width.W))
    val lnOut = Output(SInt(width.W))  // New output for natural logarithm
  })

  // --- Hyperbolic CORDIC specific constants ---
  val hyperShiftExponentsSeq: Seq[Int] = HyperCordicConstants.getHyperbolicShiftExponents(cycleCount)

  val K_H_TOTAL_ITER_GAIN_DBL: Double = HyperCordicConstants.calculateHyperbolicGainFactor(hyperShiftExponentsSeq)
  
  val INV_K_H_TOTAL_fixed = HyperCordicConstants.doubleToFixed(1.0 / K_H_TOTAL_ITER_GAIN_DBL, fractionalBits, width).S(width.W)
  val K_H_TOTAL_fixed = HyperCordicConstants.doubleToFixed(K_H_TOTAL_ITER_GAIN_DBL, fractionalBits, width).S(width.W) // Added direct K_H fixed

  val X_INIT_HYPER_fixed = if (magnitudeCorrection) {
    INV_K_H_TOTAL_fixed // This is 1.0 / K_H, so x_0 = 1/K_H
  } else {
    HyperCordicConstants.doubleToFixed(1.0, fractionalBits, width).S(width.W)
  }
  val Y_INIT_HYPER_fixed = 0.S(width.W)

  // Hyperbolic Arctan lookup table (ROM)
  val atanHyperLUT = VecInit(HyperCordicConstants.getAtanHyperLUT(fractionalBits, width, hyperShiftExponentsSeq))
  // Exponents for shifter: width of each exponent UInt should be enough for values up to 'width-1'
  val hyperShiftExponentsVec = VecInit(hyperShiftExponentsSeq.map(_.U(log2Ceil(width).W))) 

  // --- State Machine Definition ---
  object s extends ChiselEnum {
    val idle, busy, done = Value
  }
  val state = RegInit(s.idle)

  // --- Registers for CORDIC iterative values ---
  val x_reg = Reg(SInt(width.W))
  val y_reg = Reg(SInt(width.W))
  val z_reg = Reg(SInt(width.W)) 
  // iter_count goes from 0 to cycleCount-1 for iterations, then to cycleCount to signal completion
  val iter_count = Reg(UInt(log2Ceil(cycleCount + 1).W))
  val currentMode = Reg(Mode())  // Store operation mode during processing

  // --- Default output values ---
  io.done := false.B
  io.coshOut := 0.S
  io.sinhOut := 0.S
  io.atanhOut := 0.S
  io.magnitudeResultHyper := 0.S
  io.expOut := 0.S
  io.expNegOut := 0.S
  io.lnOut := 0.S  // Initialize ln output

  // --- State Machine Logic ---
  switch(state) {
    is(s.idle) {
      when(io.start) {
        currentMode := io.mode
        
        when(io.mode === Mode.AtanhMagnitudeHyper || io.mode === Mode.NaturalLog) {
          when(io.mode === Mode.NaturalLog) {
            // For ln(x), we need to compute atanh((x-1)/(x+1))
            // We'll set up x and y for the atanh computation
            val one = (BigInt(1) << fractionalBits).S(width.W)  // 1.0 in fixed point
            val xPlusOne = io.inputX + one
            val xMinusOne = io.inputX - one
            x_reg := xPlusOne  // denominator
            y_reg := xMinusOne // numerator
          }.otherwise {
            x_reg := io.inputX
            y_reg := io.inputY
          }
          z_reg := 0.S(width.W)
        }.otherwise { // Sinh/Cosh or Exponential (both use rotation mode)
          x_reg := X_INIT_HYPER_fixed 
          y_reg := Y_INIT_HYPER_fixed
          z_reg := io.targetTheta
        }
        iter_count := 0.U
        state := s.busy
      }
    }

    is(s.busy) {
      when(iter_count < cycleCount.U) {
        val current_iter_idx = iter_count 
        val current_shift_exponent = hyperShiftExponentsVec(current_iter_idx)

        val y_shifted = (y_reg >> current_shift_exponent).asSInt
        val x_shifted = (x_reg >> current_shift_exponent).asSInt
        
        val delta_theta = atanHyperLUT(current_iter_idx)
        val direction = Wire(SInt(2.W)) // +1 or -1

        when(currentMode === Mode.AtanhMagnitudeHyper || currentMode === Mode.NaturalLog) {
          // d = if (y.signum == 0) -1 else -y.signum 
          val y_sign = Mux(y_reg > 0.S, 1.S(2.W), Mux(y_reg < 0.S, -1.S(2.W), 0.S(2.W)))
          direction := Mux(y_sign === 0.S, -1.S(2.W), -y_sign)

          x_reg := x_reg + (direction * y_shifted) 
          y_reg := y_reg + (direction * x_shifted)
          z_reg := z_reg - (direction * delta_theta)
        }.otherwise { // Rotation mode (calculating Sinh/Cosh)
          // d = if (z.signum == 0) -1 else z.signum
          val z_sign = Mux(z_reg > 0.S, 1.S(2.W), Mux(z_reg < 0.S, -1.S(2.W), 0.S(2.W)))
          direction := Mux(z_sign === 0.S, -1.S(2.W), z_sign)
          
          x_reg := x_reg + (direction * y_shifted)
          y_reg := y_reg + (direction * x_shifted) 
          z_reg := z_reg - (direction * delta_theta)
        }
        iter_count := iter_count + 1.U
      }.otherwise { 
        state := s.done
      }
    }

    is(s.done) {
      io.done := true.B

      when(currentMode === Mode.AtanhMagnitudeHyper || currentMode === Mode.NaturalLog) {
        val magnitude_uncorrected = x_reg // This is Mag_true * K_H
        
        if (magnitudeCorrection) {
          val magnitude_full_prod = magnitude_uncorrected * INV_K_H_TOTAL_fixed
          io.magnitudeResultHyper := (magnitude_full_prod >> fractionalBits.U).asSInt
        } else {
          io.magnitudeResultHyper := magnitude_uncorrected
        }
        
        io.atanhOut := z_reg 
        io.coshOut := 0.S    
        io.sinhOut := 0.S   
        io.expOut := 0.S
        io.expNegOut := 0.S

        when(currentMode === Mode.NaturalLog) {
          // ln(x) = 2 * atanh((x-1)/(x+1))
          // Shift left by 1 to multiply by 2
          io.lnOut := (z_reg << 1.U).asSInt
        }.otherwise {
          io.lnOut := 0.S
        }
      }.otherwise { // SinhCosh or Exponential mode
        io.coshOut := x_reg 
        io.sinhOut := y_reg  
        io.atanhOut := 0.S 
        io.magnitudeResultHyper := 0.S 
        io.lnOut := 0.S

        when(currentMode === Mode.Exponential) {
          io.expOut := x_reg + y_reg
          io.expNegOut := x_reg - y_reg
        }.otherwise {
          io.expOut := 0.S
          io.expNegOut := 0.S
        }
      }
      assert(stable(io.coshOut))
      assert(stable(io.sinhOut))
      assert(stable(io.atanhOut))
      assert(stable(io.expOut))
      assert(stable(io.expNegOut))
      state := s.idle
    }
  }
}