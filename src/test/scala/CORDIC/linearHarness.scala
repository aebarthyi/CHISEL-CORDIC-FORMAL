package CORDIC

import chisel3._
import chisel3.util._
import chisel3.{assert, assume, cover}
import chiseltest.formal._
import CORDICCore.CORDICModeAll


class LinearHarness(val width: Int, val cycleCount: Int, val integerBits: Int = 3, val magnitudeCorrection: Boolean = true) extends Module {
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
  val dut = Module(new CORDICCore(width, cycleCount, integerBits, gainCorrection = true, includeLinear = true, includeHyperbolic = false))
  // Connecting IO instances, no initialization needed for formal
  dut.io.mode       <> io.mode
  dut.io.inputX     := io.inputX
  dut.io.inputY     := io.inputY
  dut.io.inputTheta := io.inputTheta
  io.output1        <> dut.io.output1
  io.output2        <> dut.io.output2

  // Preliminary assumptions and assertions
  // Make sure when include hyperbolic is false, that the hyperbolic modes are never requested
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
  }
  assume(io.mode.bits === CORDICCore.CORDICModeAll.LinearMultiply || io.mode.bits === CORDICCore.CORDICModeAll.LinearDivide,
         "Only LinearMultiply and LinearDivide modes should be used in LinearHarness.")
  when (io.mode.bits === CORDICCore.CORDICModeAll.LinearMultiply) {
    assume(io.inputTheta === 0.S, "inputTheta should be zero in LinearMultiply mode")
  } .elsewhen (io.mode.bits === CORDICCore.CORDICModeAll.LinearDivide) {
    assume(io.inputTheta === 0.S, "inputTheta should be zero in LinearDivide mode")
    assume(io.inputY =/= 0.S, "inputY (divisor) should not be zero in LinearDivide mode")
  }
}