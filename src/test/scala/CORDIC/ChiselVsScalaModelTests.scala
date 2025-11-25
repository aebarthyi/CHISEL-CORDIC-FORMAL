// This file contains tests that compare the Chisel hardware implementation of CORDIC algorithms against a Scala software model.
package CORDIC

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math._

class ChiselVsScalaModelTests extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  val testWidth = 32
  val testCycles = 15
  val testIntegerBits = 3
  val testFractionalBits = testWidth - 1 - testIntegerBits
  val tolerance = 0.02

  def doubleToFixed(value: Double): BigInt =
    CordicModelConstants.doubleToFixed(value, testFractionalBits, testWidth)

  def fixedToDouble(value: BigInt): Double = {
    value.toDouble / (1L << testFractionalBits)
  }

  /**
    * Runs a single trigonometric CORDIC test, comparing Chisel DUT against a Scala model.
    * @param dut The CordicTrig instance (device under test).
    * @param model The TrigCordicModel instance (Scala model for reference).
    * @param mode The operation mode (SinCos or ArctanMagnitude).
    * @param angle The input angle for SinCos mode.
    * @param xIn The input x-coordinate for ArctanMagnitude mode.
    * @param yIn The input y-coordinate for ArctanMagnitude mode.
    */
  def runTrigTest(
      dut: CordicTrig,
      model: TrigCordicModel,
      mode: CordicTrigConstants.Mode.Type,
      angle: Double = 0.0,
      xIn: Double = 0.0,
      yIn: Double = 0.0
  ): Unit = {
    val angleFixed = doubleToFixed(angle)
    val xFixed = doubleToFixed(xIn)
    val yFixed = doubleToFixed(yIn)

    model.reset()
    model.setInputs(
      start = true,
      modeIn = if (mode == CordicTrigConstants.Mode.SinCos) CordicModelConstants.Mode.SinCos
      else CordicModelConstants.Mode.ArctanMagnitude,
      theta = angleFixed,
      xIn = xFixed,
      yIn = yFixed
    )
    while (!model.done) model.step()

    dut.io.start.poke(true.B)
    dut.io.mode.poke(mode)
    dut.io.targetTheta.poke(angleFixed.S)
    dut.io.inputX.poke(xFixed.S)
    dut.io.inputY.poke(yFixed.S)
    dut.clock.step(1)
    dut.io.start.poke(false.B)

    var timeout = 0
    while (!dut.io.done.peek().litToBoolean && timeout < 50) {
      dut.clock.step(1)
      timeout += 1
    }
    dut.io.done.expect(true.B)

    if (mode == CordicTrigConstants.Mode.SinCos) {
      fixedToDouble(dut.io.cosOut.peek().litValue) should be(fixedToDouble(model.cos) +- tolerance)
      fixedToDouble(dut.io.sinOut.peek().litValue) should be(fixedToDouble(model.sin) +- tolerance)
    } else {
      fixedToDouble(dut.io.arctanOut.peek().litValue) should be(fixedToDouble(model.arctan) +- tolerance)
      fixedToDouble(dut.io.magnitudeOut.peek().litValue) should be(fixedToDouble(model.magnitude) +- tolerance)
    }
    dut.clock.step(2)
  }

  behavior of "TrigChiselVsScala"

  it should "calculate sin/cos correctly and match Scala model" in {
    test(new CordicTrig(testWidth, testCycles, testIntegerBits)) { dut =>
      val model = new TrigCordicModel(testWidth, testCycles, testIntegerBits)
      val testAngles = Seq(0.0, Pi / 6, Pi / 4, Pi / 3, Pi / 2)
      for (angle <- testAngles) {
        runTrigTest(dut, model, CordicTrigConstants.Mode.SinCos, angle = angle)
      }
    }
  }

  it should "calculate arctan and magnitude correctly and match Scala model" in {
    test(new CordicTrig(testWidth, testCycles, testIntegerBits)) { dut =>
      val model = new TrigCordicModel(testWidth, testCycles, testIntegerBits)
      val testCoords = Seq((1.0, 0.0), (1.0, 1.0), (1.0, 0.5))
      for ((x, y) <- testCoords) {
        runTrigTest(dut, model, CordicTrigConstants.Mode.ArctanMagnitude, xIn = x, yIn = y)
      }
    }
  }

  /**
    * Runs a single linear CORDIC test, comparing Chisel DUT against a Scala model.
    * @param dut The LinearCordic instance (device under test).
    * @param model The LinearCordicModel instance (Scala model for reference).
    * @param mode The operation mode (Multiply or Divide).
    * @param aIn The first input value (multiplicand or dividend).
    * @param bIn The second input value (multiplier or divisor).
    */
  def runLinearTest(
      dut: LinearCordic,
      model: LinearCordicModel,
      mode: LinearCordicConstants.Mode.Type,
      aIn: Double,
      bIn: Double
  ): Unit = {
    val aFixed = doubleToFixed(aIn)
    val bFixed = doubleToFixed(bIn)

    model.reset()
    model.setInputs(
      start = true,
      modeIn = if (mode == LinearCordicConstants.Mode.Multiply) CordicModelConstants.ModeLinear.Multiply
      else CordicModelConstants.ModeLinear.Divide,
      a = aFixed,
      b = bFixed
    )
    while (!model.done) model.step()

    dut.io.start.poke(true.B)
    dut.io.mode.poke(mode)
    dut.io.inputA.poke(aFixed.S)
    dut.io.inputB.poke(bFixed.S)
    dut.clock.step(1)
    dut.io.start.poke(false.B)

    var timeout = 0
    while (!dut.io.done.peek().litToBoolean && timeout < 50) {
      dut.clock.step(1)
      timeout += 1
    }
    dut.io.done.expect(true.B)

    if (mode == LinearCordicConstants.Mode.Multiply) {
      fixedToDouble(dut.io.productResult.peek().litValue) should be(fixedToDouble(model.product) +- tolerance)
    } else {
      fixedToDouble(dut.io.quotientResult.peek().litValue) should be(fixedToDouble(model.quotient) +- tolerance)
    }
    dut.clock.step(2)
  }

  behavior of "LinearChiselVsScala"

  it should "calculate multiplication correctly and match Scala model" in {
    test(new LinearCordic(testWidth, testCycles, testIntegerBits)) { dut =>
      val model = new LinearCordicModel(testWidth, testCycles, testIntegerBits)
      val testValues = Seq((2.5, 3.0), (-2.5, 3.0), (0.125, 0.5))
      for ((a, b) <- testValues) {
        runLinearTest(dut, model, LinearCordicConstants.Mode.Multiply, a, b)
      }
    }
  }

  it should "calculate division correctly and match Scala model" in {
    test(new LinearCordic(testWidth, testCycles, testIntegerBits)) { dut =>
      val model = new LinearCordicModel(testWidth, testCycles, testIntegerBits)
      val testValues = Seq((7.5, 2.5), (-7.5, 2.5), (1.0, 4.0))
      for ((a, b) <- testValues) {
        runLinearTest(dut, model, LinearCordicConstants.Mode.Divide, a, b)
      }
    }
  }

  /**
    * Runs a single hyperbolic CORDIC test, comparing Chisel DUT against a Scala model.
    * @param dut The HyperCordic instance (device under test).
    * @param model The HyperCordicModel instance (Scala model for reference).
    * @param mode The operation mode (SinhCosh or AtanhMagnitudeHyper).
    * @param theta The input theta for SinhCosh mode.
    * @param xIn The input x-coordinate for AtanhMagnitudeHyper mode.
    * @param yIn The input y-coordinate for AtanhMagnitudeHyper mode.
    */
  def runHyperTest(
      dut: HyperCordic,
      model: HyperCordicModel,
      mode: HyperCordicConstants.Mode.Type,
      theta: Double = 0.0,
      xIn: Double = 0.0,
      yIn: Double = 0.0
  ): Unit = {
    val thetaFixed = doubleToFixed(theta)
    val xFixed = doubleToFixed(xIn)
    val yFixed = doubleToFixed(yIn)

    model.reset()
    val modelMode = mode match {
      case HyperCordicConstants.Mode.SinhCosh => CordicModelConstants.ModeHyper.SinhCosh
      case HyperCordicConstants.Mode.AtanhMagnitudeHyper => CordicModelConstants.ModeHyper.AtanhMagnitudeHyper
      case HyperCordicConstants.Mode.Exponential => CordicModelConstants.ModeHyper.Exponential
      case HyperCordicConstants.Mode.NaturalLog => CordicModelConstants.ModeHyper.NaturalLog
    }

    model.setInputs(
      start = true,
      modeIn = modelMode,
      theta = thetaFixed,
      xIn = xFixed,
      yIn = yFixed
    )
    while (!model.done) model.step()
    
    dut.io.mode.poke(mode)
    dut.io.start.poke(true.B)
    
    // Poke inputs based on the mode
    mode match {
      case HyperCordicConstants.Mode.SinhCosh | HyperCordicConstants.Mode.Exponential =>
        dut.io.targetTheta.poke(thetaFixed.S)
        dut.io.inputX.poke(0.S)
        dut.io.inputY.poke(0.S)
      case HyperCordicConstants.Mode.AtanhMagnitudeHyper =>
        dut.io.targetTheta.poke(0.S)
        dut.io.inputX.poke(xFixed.S)
        dut.io.inputY.poke(yFixed.S)
      case HyperCordicConstants.Mode.NaturalLog =>
        dut.io.targetTheta.poke(0.S)
        dut.io.inputX.poke(xFixed.S)
        dut.io.inputY.poke(0.S)
    }

    dut.clock.step(1)
    dut.io.start.poke(false.B)

    var timeout = 0
    while (!dut.io.done.peek().litToBoolean && timeout < 50) {
      dut.clock.step(1)
      timeout += 1
    }
    dut.io.done.expect(true.B)

    mode match {
      case HyperCordicConstants.Mode.SinhCosh =>
        val dutCosh = dut.io.coshOut.peek().litValue
        val dutSinh = dut.io.sinhOut.peek().litValue
        fixedToDouble(dutCosh) should be(fixedToDouble(model.cosh) +- tolerance)
        fixedToDouble(dutSinh) should be(fixedToDouble(model.sinh) +- tolerance)
      case HyperCordicConstants.Mode.AtanhMagnitudeHyper =>
        val dutAtanh = dut.io.atanhOut.peek().litValue
        val dutMag = dut.io.magnitudeResultHyper.peek().litValue
        fixedToDouble(dutAtanh) should be(fixedToDouble(model.atanh) +- tolerance)
        fixedToDouble(dutMag) should be(fixedToDouble(model.magnitudeHyper) +- tolerance)
      case HyperCordicConstants.Mode.Exponential =>
        val dutExp = dut.io.expOut.peek().litValue
        val dutExpNeg = dut.io.expNegOut.peek().litValue
        fixedToDouble(dutExp) should be(fixedToDouble(model.exp) +- tolerance)
        fixedToDouble(dutExpNeg) should be(fixedToDouble(model.expNeg) +- tolerance)
      case HyperCordicConstants.Mode.NaturalLog =>
        val dutLn = dut.io.lnOut.peek().litValue
        fixedToDouble(dutLn) should be(fixedToDouble(model.ln) +- tolerance)
    }

    dut.clock.step(2)
  }

  behavior of "HyperChiselVsScala"

  it should "calculate sinh/cosh correctly and match Scala model" in {
    test(new HyperCordic(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)) { dut =>
      val model = new HyperCordicModel(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)
      val testThetas = Seq(0.0, 0.5, 1.0, -0.5)
      for (theta <- testThetas) {
        runHyperTest(dut, model, HyperCordicConstants.Mode.SinhCosh, theta = theta)
      }
    }
  }

  it should "calculate atanh/magnitude correctly and match Scala model" in {
    test(new HyperCordic(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)) { dut =>
      val model = new HyperCordicModel(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)
      val testCoords = Seq((1.2, 0.5), (2.0, 1.0), (1.5, -0.3))
      for ((x, y) <- testCoords) {
        if (x > abs(y)) {
          runHyperTest(dut, model, HyperCordicConstants.Mode.AtanhMagnitudeHyper, xIn = x, yIn = y)
        }
      }
    }
  }

  it should "calculate e^x and e^-x correctly and match Scala model" in {
    test(new HyperCordic(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)) { dut =>
      val model = new HyperCordicModel(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)
      val testThetas = Seq(0.0, 0.5, 1.0, -0.5, 0.25)
      for (theta <- testThetas) {
        runHyperTest(dut, model, HyperCordicConstants.Mode.Exponential, theta = theta)
      }
    }
  }

  it should "calculate ln(x) correctly and match Scala model" in {
    test(new HyperCordic(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)) { dut =>
      val model = new HyperCordicModel(testWidth, testCycles, testIntegerBits, magnitudeCorrection = true)
      // Note: input range for ln(x) is restricted by the vectoring mode convergence
      // ln(x) = 2*atanh((x-1)/(x+1)). For convergence, |(x-1)/(x+1)| < ~0.8
      // This means x should be roughly between 0.11 and 9.
      val testValues = Seq(0.2, 0.5, 1.0, 2.0, 4.0)
      for (x <- testValues) {
        runHyperTest(dut, model, HyperCordicConstants.Mode.NaturalLog, xIn = x)
      }
    }
  }
}