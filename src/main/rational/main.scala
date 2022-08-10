import rational.{Rational}


@main def main(): Unit = {
  val a = new Rational(1)
  val b = new Rational(2)
  println((a + b).gcd)
}