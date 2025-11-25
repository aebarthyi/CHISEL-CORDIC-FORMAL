package CORDIC

import chisel3._
import chisel3.util._
import chisel3.{assert, assume, cover}
import chiseltest.formal._
import CORDICCore.CORDICModeAll

class TrigHarness(val width: Int, val cycleCount: Int, val integerBits: Int, val magnitudeCorrection: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val mode        = Flipped(Decoupled(CORDICModeAll()))
    val inputX      = Input(SInt(width.W))
    val inputY      = Input(SInt(width.W))
    val inputTheta  = Input(SInt(width.W))
    val output1     = Decoupled(SInt(width.W))
    val output2     = Decoupled(SInt(width.W))
  })

  // Test parameters
  val fractionalBits = width - 1 - integerBits
  val tolerance = 0.001 // Tolerance for floating point comparisons

  // Identical instance as trig tests
  val dut = Module(new CORDICCore(width, cycleCount, integerBits, gainCorrection = true, includeLinear = false, includeHyperbolic = false))

  // Connecting IO instances, no initialization needed for formal
  dut.io.mode       <> io.mode
  dut.io.inputX     := io.inputX
  dut.io.inputY     := io.inputY
  dut.io.inputTheta := io.inputTheta
  io.output1        <> dut.io.output1
  io.output2        <> dut.io.output2

  // Preliminary assumptions and assertions
  // Make sure when include linear and include hyperbolic are false, that the following modes are never requested
  when (dut.io.mode.fire) {
    when (!dut.includeHyperbolic.B) {
      assume(
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.HyperSinhCosh &&
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.HyperAtanhMagnitude &&
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.Exponential &&
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.NaturalLog,
        "The hyperbolic modes should not be requested when includeHyperbolic is false."
      )
    }
    when (!dut.includeLinear.B) {
      assume(
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.LinearMultiply &&
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.LinearDivide,
        "The linear modes should not be requested when includeLinear is false."
      )
    }
  }
  assume(io.mode.bits === CORDICCore.CORDICModeAll.TrigSinCos ||
      io.mode.bits === CORDICCore.CORDICModeAll.TrigArctanMagnitude,
      "Only TrigSinCos and TrigArctanMagnitude modes should be used in TrigHarness.")

  when (io.mode.bits === CORDICCore.CORDICModeAll.TrigSinCos) {
    val scale = BigInt(1) << fractionalBits

    val minThetaReal   = -1.74
    val maxThetaReal   =  1.74

    val minThetaRaw    = BigInt(math.round(minThetaReal * scale.toDouble))
    val maxThetaRaw    = BigInt(math.round(maxThetaReal * scale.toDouble))

    val minThetaSInt   = minThetaRaw.S(width.W)
    val maxThetaSInt   = maxThetaRaw.S(width.W)

    assume(io.inputX === 0.S, "inputX should be zero in TrigSinCos mode")
    assume(io.inputY === 0.S, "inputY should be zero in TrigSinCos mode")
    assume(io.inputTheta >= minThetaSInt && io.inputTheta <= maxThetaSInt, "inputTheta should be within the valid range in TrigSinCos mode")
  }
  .elsewhen (io.mode.bits === CORDICCore.CORDICModeAll.TrigArctanMagnitude) {
    assume(io.inputX >= 0.S, "inputX should be positive in TrigArctanMagnitude mode")
    assume(io.inputTheta === 0.S, "inputTheta should be zero in TrigArctanMagnitude mode")
  }
}
