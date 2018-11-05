package fintech.homework04

import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.{ComplexNumber, Eq}
import fintech.homework06.Eq._

class EqSpec extends FlatSpec with Matchers {

  "Eq" should "is correct with Map" in {
    Map(1 -> 5) ==== Map(1 -> 5) should be(true)
    Map(1 -> 5) ==== Map(1 ->8) should be(false)
  }

  "Eq" should "is correct with Seq" in {
    Seq(1, 2) ==== Seq(1, 2) should be(true)
    Seq(5) ==== Seq(8) should be(false)

  }

  "Eq" should "is correct with Option" in {
    Option(5) ==== Option(8) should be(false)
    Option(1) ==== Option(1) should be(true)
  }

  "Eq" should "is correct with Complex Numbers" in {
    new ComplexNumber(1, 2) ==== new ComplexNumber(1, 2) should be(true)
    new ComplexNumber(5.8, 1.3) ==== new ComplexNumber(1.3, 6.8) should be(false)
  }


}
