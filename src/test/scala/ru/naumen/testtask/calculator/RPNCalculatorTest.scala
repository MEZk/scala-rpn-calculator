package ru.naumen.testtask.calculator

import org.scalatest.FlatSpec

class RPNCalculatorTest extends FlatSpec {

  "The compute method with input '1+1'" should "return 2" in {
    assertResult(BigDecimal("2")) {
      val calculator = new RPNCalculator()
      calculator.compute("1+1")
    }
  }

  "The compute method with input '-1+1'" should "return 0" in {
    assertResult(BigDecimal("0")) {
      val calculator = new RPNCalculator()
      calculator.compute("-1+1")
    }
  }

  "The compute method with input '2-1'" should "return 1" in {
    assertResult(BigDecimal("1")) {
      val calculator = new RPNCalculator()
      calculator.compute("2-1")
    }
  }

  "The compute method with input '50000!'" should "return 1.665977451987763384745382675296579E+106624" in {
    assertResult(BigDecimal("1.665977451987763384745382675296579E+106624")) {
      val calculator = new RPNCalculator()
      calculator.compute("50000!")
    }
  }

  "The compute method with input '3^2'" should "return 9" in {
    assertResult(BigDecimal("9")) {
      val calculator = new RPNCalculator()
      calculator.compute("3^2")
    }
  }

  "The compute method with input '2*2'" should "return 4" in {
    assertResult(BigDecimal("4")) {
      val calculator = new RPNCalculator()
      calculator.compute("2*2")
    }
  }

  "The compute method with input '6/4'" should "return 1.5" in {
    assertResult(BigDecimal("1.5")) {
      val calculator = new RPNCalculator()
      calculator.compute("6/4")
    }
  }

  "The compute method with input 'sin(3)'" should "return 0.1411200080598672" in {
    assertResult(BigDecimal("0.1411200080598672")) {
      val calculator = new RPNCalculator()
      calculator.compute("sin(3)")
    }
  }

  "The compute method with input 'cos(3)'" should "return -0.9899924966004454" in {
    assertResult(BigDecimal("-0.9899924966004454")) {
      val calculator = new RPNCalculator()
      calculator.compute("cos(3)")
    }
  }

  "The compute method with input '12 + 45.0 - 0.34'" should "return 56.66" in {
    assertResult(56.66) {
      val calculator = new RPNCalculator()
      calculator.compute("12 + 45.0 - 0.34")
    }
  }

  "The compute method with input '1 + 2 * 3 / 4.4 - 2 ^ 3'" should "return -5.636363636363636363636363636363636" in {
    assertResult(BigDecimal("-5.636363636363636363636363636363636")) {
      val calculator = new RPNCalculator()
      calculator.compute("1 + 2 * 3 / 4.4 - 2 ^ 3")
    }
  }

  "The compute method with input '-(23 + 1/4) * 2! + sin(cos(1 + 2^4))'" should "return -46.77170413398965854" in {
    assertResult(BigDecimal("-46.77170413398965854")) {
      val calculator = new RPNCalculator()
      calculator.compute("-(23 + 1/4) * 2! + sin(cos(1 + 2^4))")
    }
  }

  "The compute method with input '2*2+(18-48)*(sin(4)+cos(5))-10^(8/2)'" should "return -9981.80579070465894150" in {
    assertResult(BigDecimal("-9981.80579070465894150")) {
      val calculator = new RPNCalculator()
      calculator.compute("2*2+(18-48)*(sin(4)+cos(5))-10^(8/2)")
    }
  }

  "The compute method with input 'sin(111111111111111111111111111111111111111111111111111111111)'" should "return -0.23881086583869462" in {
    assertResult(BigDecimal("-0.23881086583869462")) {
      val calculator = new RPNCalculator()
      calculator.compute("sin(111111111111111111111111111111111111111111111111111111111)")
    }
  }

  "The compute method" should "produce IllegalArgumentException when input contains invalid symbol" in {
    intercept[IllegalArgumentException] {
      val calculator = new RPNCalculator()
      calculator.compute("$2+1")
    }
  }

  "The compute method" should "produce NoSuchElementException when input contains expression which has wrong format" in {
    intercept[NoSuchElementException] {
      val calculator = new RPNCalculator()
      calculator.compute("21+")
    }
  }

  "The compute method" should "produce IllegalArgumentException when closing bracket was missed"in {
    intercept[IllegalArgumentException] {
      val calculator = new RPNCalculator()
      calculator.compute("sin(5")
    }
  }

  it should "also produce NoSuchElementException when opening bracket was missed" in {
    intercept[NoSuchElementException] {
      val calculator = new RPNCalculator()
      calculator.compute("(2+1))")
    }
  }
}
