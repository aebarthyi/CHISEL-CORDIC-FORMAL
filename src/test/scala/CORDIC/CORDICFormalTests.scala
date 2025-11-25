package CORDIC

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math._


class CORDICFormalTests extends AnyFlatSpec with ChiselScalatestTester with Matchers with Formal {
  behavior of "FormalTrigCordic"

  it should "pass bounded formal check" in {
    verify(new TrigHarness(32, 16, 3), Seq(BoundedCheck(10)))
  }

  behavior of "FormalLinearCordic"

  it should "pass bounded formal check" in {
    verify(new LinearHarness(32, 16, 3), Seq(BoundedCheck(10)))
  }

  behavior of "FormalHyperCordic"

  it should "pass bounded formal check" in {
    verify(new HyperHarness(32, 16, 3), Seq(BoundedCheck(10)))
  }
}