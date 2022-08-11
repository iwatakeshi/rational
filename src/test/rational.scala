import org.scalatest.funspec.AnyFunSpec
import rational.Rational
import rational.Extensions.*

class RationalSpec extends AnyFunSpec {
  describe("Rational") {
    describe("Extensions") {
      describe("Int.toRational") {
        it("should convert an integer to a rational number") {
          val a = 1.toRational
          assert(a.numerator == 1 && a.denominator == 1)
        }
      }

      describe("(Int, Int).toRational") {
       it("should convert a tuple to a rational number") {
         val a = (1, 2).toRational
         assert(a.numerator == 1 && a.denominator == 2)
       }
      }
    }

    describe("toDouble") {
      it("should convert a rational number to double") {
        assert(new Rational(1, 2).toDouble == 0.5)
      }
    }

    describe("toTuple") {
      it("should convert a rational number to a tuple") {
        assert(new Rational(1, 2).toTuple == (1, 2))
      }
    }

    describe("flip") {
      it("should flip the numerator and denominator") {
        val a = new Rational(1, 2).flip
        assert(a.numerator == 2 && a.denominator == 1)
      }
    }

    describe("reduce") {
      it("should reduce the rational number") {
        val a = new Rational(5, 10).reduce
        assert(a.numerator == 1 && a.denominator == 2)
      }
    }

    describe("gcd") {
      it("should return the greatest common denominator") {
        val a = new Rational(5, 10).gcd
        assert(a == 5)
      }
    }

    describe(".fromTuple") {
      it("should convert a tuple of (Int, Int) to a rational number") {
        assert(Rational.fromTuple(1, 2) == new Rational(1, 2))
      }
    }

    describe("+") {
      it ("should add two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1, 2)
        assert((a + b) == 1.0)
      }
    }
    describe("-") {
      it("should subtract two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1, 2)
        assert((a - b) == 0.0)
      }
    }

    describe("*") {
      it("should multiply two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(2, 1)
        assert((a * b) == 1.0)
      }
    }

    describe("/") {
      it("should divide two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1, 2)
        assert((a / b) == 1.0)
      }
    }

    describe("<") {
      it("should compare two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1)
        assert((a < b) == true)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(1, 2)
        val b = 1.0
        assert((a < b) == true)
      }
    }

    describe("<=") {
      it("should compare two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1)
        assert((a <= b) == true)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(1, 2)
        val b = 1.0
        assert((a <= b) == true)
      }
    }

    describe(">") {
      it("should compare two rational numbers") {
        val a = new Rational(2, 3)
        val b = new Rational(1, 2)

        assert((a > b) == true)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(2, 3)
        val b = 0.5
        assert((a > b) == true)
      }
    }

    describe(">=") {
      it("should compare two rational numbers") {
        val a = new Rational(2, 3)
        val b = new Rational(1, 2)

        assert((a >= b) == true)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(2, 3)
        val b = 0.5
        assert((a >= b) == true)
      }
    }

    describe("==") {
      it("should compare two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1, 2)
        assert((a == b) == true)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(1, 2)
        assert((a == 0.5) == true)
      }
    }

    describe("!=") {
      it("should compare two rational numbers") {
        val a = new Rational(1, 2)
        val b = new Rational(1, 2)
        assert((a != b) == false)
      }

      it("should compare a rational number and a double") {
        val a = new Rational(1, 2)
        assert((a != 0.5) == false)
      }
    }

    describe("min") {
      it("should return the minimum rational number") {
        val a = new Rational(1, 2)
        val b = new Rational(1)
        assert(a.min(b) == a)
      }
    }

    describe("max") {
      it("should return the minimum rational number") {
        val a = new Rational(1, 2)
        val b = new Rational(1)
        assert(a.max(b) == b)
      }
    }
  }
}