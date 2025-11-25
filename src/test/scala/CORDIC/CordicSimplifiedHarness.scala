package CORDIC

import chisel3._
import chisel3.util._
import chisel3.{assert, assume, cover}
import chiseltest.formal._

object CORDICModeAll extends ChiselEnum {
val TrigSinCos, TrigArctanMagnitude,
    LinearMultiply, LinearDivide,
    HyperSinhCosh, HyperAtanhMagnitude,
    Exponential, NaturalLog = Value
}


class CordicSimplifiedFormalHarness(val width: Int, val cycleCount: Int, val integerBits: Int = 3, val magnitudeCorrection: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val mode        = Flipped(Decoupled(CORDICModeAll()))
    val inputX      = Input(SInt(width.W))
    val inputY      = Input(SInt(width.W))
    val inputTheta  = Input(SInt(width.W))
    val output1     = Decoupled(SInt(width.W))
    val output2     = Decoupled(SInt(width.W))
  })

  // Test parameters
  val testWidth = 32
  val testIntegerBits = 3 // Gives 32-1-3 = 28 fractional bits
  val testFractionalBits = testWidth - 1 - testIntegerBits
  val testCycleCount = 16 // Sufficient for reasonable precision
  val tolerance = 0.001 // Tolerance for floating point comparisons

  // Identical instance as trig tests
  val dut = Module(new CORDICCore(testWidth, testCycleCount, testIntegerBits, gainCorrection = true, includeLinear = false, includeHyperbolic = false))

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
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.NaturalLog
      )
      assert(dut.io.output1.bits === 0.S) // Purposefully failing to test harness
    }

    when (!dut.includeLinear.B) {
      assume(
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.LinearMultiply &&
        dut.io.mode.bits =/= CORDICCore.CORDICModeAll.LinearDivide
      )
    }
  }
}
