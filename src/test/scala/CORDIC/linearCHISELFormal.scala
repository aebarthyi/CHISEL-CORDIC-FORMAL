package CORDIC

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._

class LinearCordicFormal(val width: Int, val cycleCount: Int, val integerBits: Int = 3) extends Module {
  import LinearCordicConstants._
  import LinearCordicConstants.Mode._

  // Parameter Validations
  require(width > 0, "Width must be positive")
  require(cycleCount > 0, "Cycle count must be positive")
  require(integerBits >= 1, "Integer bits must be at least 1 (for sign or small numbers)")
  val fractionalBits: Int = width - 1 - integerBits
  require(fractionalBits > 0, s"Fractional bits must be positive. Check width ($width) vs integerBits ($integerBits). FractionalBits = $fractionalBits")

  val io = IO(new Bundle {
    // Control
    val start = Input(Bool())
    val mode = Input(Mode())

    // Data Inputs
    val inputA = Input(SInt(width.W)) // For Multiply: Multiplicand A; For Divide: Dividend A
    val inputB = Input(SInt(width.W)) // For Multiply: Multiplier B;  For Divide: Divisor B

    // Data Outputs
    val done = Output(Bool())
    val productResult = Output(SInt(width.W))
    val quotientResult = Output(SInt(width.W))
  })

  // --- Fixed-point constants ---
  val ONE_FIXED = doubleToFixed(1.0, fractionalBits, width).S(width.W)
  val CORDIC_RANGE_LIMIT_R_FIXED = doubleToFixed(CORDIC_RANGE_LIMIT_R, fractionalBits, width).S(width.W)

  // --- State Machine Definition ---
  object s extends ChiselEnum {
    val idle, busy, done = Value
  }
  val state = RegInit(s.idle)

  // --- Registers for CORDIC iterative values ---
  val x_reg = Reg(SInt(width.W)) // Holds constant multiplicand or abs(divisor)
  val y_reg = Reg(SInt(width.W)) // Accumulator for product or scaled dividend
  val z_reg = Reg(SInt(width.W)) // Holds scaled multiplier or accumulates quotient

  val iter_count = Reg(UInt(log2Ceil(cycleCount + 1).W))
  val currentMode_reg = Reg(Mode())
  val currentScalingFactorK_reg = Reg(UInt(log2Ceil(width + 1).W)) // Max k is 'width'
  val originalDivisorIsNegative_reg = Reg(Bool())

  // Registers for progressively shifted terms
  val termX_shifted_reg = Reg(SInt(width.W))
  val termOne_shifted_reg = Reg(SInt(width.W))

  // --- Output Registers ---
  val productResult_reg = Reg(SInt(width.W))
  val quotientResult_reg = Reg(SInt(width.W))
  val done_reg = RegInit(false.B)

  io.done := done_reg
  io.productResult := productResult_reg
  io.quotientResult := quotientResult_reg

  // --- Clamp helper function ---
  def clamp(value: SInt, targetWidth: Int): SInt = {
    val maxVal_target = ((BigInt(1) << (targetWidth - 1)) - 1).S(targetWidth.W)
    val minVal_target = (-(BigInt(1) << (targetWidth - 1))).S(targetWidth.W)

    // Extend target min/max to width of value for comparison
    val value_w = value.getWidth
    val max_val_extended = maxVal_target.pad(value_w)
    val min_val_extended = minVal_target.pad(value_w)

    Mux(value > max_val_extended, maxVal_target,
      Mux(value < min_val_extended, minVal_target,
        value(targetWidth - 1, 0).asSInt)) // Truncate/select lower bits
  }

  // --- State Machine Logic ---
  switch(state) {
    is(s.idle) {
      done_reg := false.B
      productResult_reg := 0.S
      quotientResult_reg := 0.S
      originalDivisorIsNegative_reg := false.B

      when(io.start) {
        currentMode_reg := io.mode
        iter_count := 0.U

        val inputA_abs = io.inputA.abs.asUInt
        val inputB_abs = io.inputB.abs.asUInt

        val calculated_k_val = Wire(UInt(log2Ceil(width + 1).W))
        val final_x_val = Wire(SInt(width.W))
        val final_y_val = Wire(SInt(width.W))
        val final_z_val = Wire(SInt(width.W))

        when(io.mode === Mode.Multiply) {
          // Calculate k for scaling inputB (multiplier)
          // Smallest k such that (inputB_abs >> k) <= CORDIC_RANGE_LIMIT_R_FIXED.asUInt
          val conditions_mult = VecInit(Seq.tabulate(width + 1) { k_idx =>
            (inputB_abs >> k_idx) <= CORDIC_RANGE_LIMIT_R_FIXED.asUInt
          })
          val k_mult = PriorityEncoder(conditions_mult)
          calculated_k_val := k_mult

          final_x_val := io.inputA // Multiplicand
          final_y_val := 0.S // Accumulator for product
          final_z_val := io.inputB >> k_mult // Scaled Multiplier
        } .otherwise { // Mode.Divide
          // Pre-check for division by zero (handled by require in Scala, user must ensure B!=0)
          originalDivisorIsNegative_reg := io.inputB < 0.S
          val absDivisor_uint = inputB_abs // abs(Divisor)

          // Calculate limitForAScaledAbs = (absDivisor * CORDIC_RANGE_LIMIT_R_FIXED) >> fractionalBits
          val limit_prod_intermediate = absDivisor_uint.zext * CORDIC_RANGE_LIMIT_R_FIXED.asUInt.zext // Wider intermediate
          val limitForAScaledAbs_UInt = (limit_prod_intermediate >> fractionalBits).asUInt // Assume positive result

          // Calculate k for scaling inputA (dividend)
          // Smallest k such that (inputA_abs >> k) <= limitForAScaledAbs_UInt
          // Or if limitForAScaledAbs_UInt is 0 and inputA_abs is not, then (inputA_abs >> k) === 0.U
          val conditions_div = VecInit(Seq.tabulate(width + 1) { k_idx =>
            Mux(limitForAScaledAbs_UInt === 0.U && inputA_abs =/= 0.U,
              (inputA_abs >> k_idx) === 0.U,
              (inputA_abs >> k_idx) <= limitForAScaledAbs_UInt)
          })
          val k_div = PriorityEncoder(conditions_div)
          calculated_k_val := k_div

          final_x_val := absDivisor_uint.asSInt // x is abs(Divisor)
          final_y_val := io.inputA >> k_div // Scaled Dividend (A')
          final_z_val := 0.S // Accumulator for quotient
        }

        currentScalingFactorK_reg := calculated_k_val
        x_reg := final_x_val
        y_reg := final_y_val
        z_reg := final_z_val

        // Initialize shift registers for the first iteration
        termX_shifted_reg := final_x_val  // This is x_reg >> 0
        termOne_shifted_reg := ONE_FIXED // This is ONE_FIXED >> 0

        state := s.busy
      }
    }

    is(s.busy) {
      when(iter_count < cycleCount.U) {
        // val current_iter = iter_count // No longer needed for these shifts

        // Linear CORDIC shifts: use pre-shifted values from registers
        val termXShifted = termX_shifted_reg
        val termOneShifted = termOne_shifted_reg

        val direction = Wire(SInt(2.W)) // +1 or -1

        when(currentMode_reg === Mode.Divide) {
          // For division: y is residual, z is quotient. d = -sign(y) to drive y to 0.
          // Scala: if (y == 0) -1 else -y.signum
          val y_sign = Mux(y_reg > 0.S, 1.S(2.W), Mux(y_reg < 0.S, -1.S(2.W), 0.S(2.W)))
          direction := Mux(y_sign === 0.S, -1.S(2.W), -y_sign) // Matches Scala: if y_reg is 0, d is -1.
        } .otherwise { // Mode.Multiply
          // For multiplication: y is product, z is residual multiplier. d = sign(z) to drive z to 0.
          // Scala: if (z == 0) 1 else z.signum
          val z_sign = Mux(z_reg > 0.S, 1.S(2.W), Mux(z_reg < 0.S, -1.S(2.W), 0.S(2.W)))
          direction := Mux(z_sign === 0.S, 1.S(2.W), z_sign) // Matches Scala: if z_reg is 0, d is 1.
        }

        // Update equations from Scala model:
        // y_new = y + direction * termXShifted
        // z_new = z - direction * termOneShifted
        y_reg := y_reg + (direction * termXShifted)
        z_reg := z_reg - (direction * termOneShifted)

        // Update shifted value registers for the next iteration
        termX_shifted_reg := (termX_shifted_reg >> 1).asSInt
        termOne_shifted_reg := (termOne_shifted_reg >> 1).asSInt

        iter_count := iter_count + 1.U
      } .otherwise {
        state := s.done
      }
    }

    is(s.done) {
      val k = currentScalingFactorK_reg
      when(currentMode_reg === Mode.Multiply) {
        // rawResult = y_reg (A * B_scaled)
        // product = (A * B_scaled) * 2^k = A * B
        val shifted_prod = y_reg << k // Result width can be y_reg.width + k_max
        productResult_reg := clamp(shifted_prod, width)
      } .otherwise { // Mode.Divide
        // rawResult = z_reg (A_scaled / abs(B))
        // quotient = (A_scaled / abs(B)) * 2^k = A / abs(B)
        val shifted_quot_base = z_reg << k
        val finalQuotient_value = Mux(originalDivisorIsNegative_reg, -shifted_quot_base, shifted_quot_base)
        quotientResult_reg := clamp(finalQuotient_value, width)
      }
      assert(stable(io.productResult))
      assert(stable(io.quotientResult))
      done_reg := true.B
      state := s.idle
    }
  }
} 