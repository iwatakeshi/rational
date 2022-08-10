package rational
import scala.annotation.targetName

object Extensions {
  extension (a: Int) {
    def toRational = new Rational(a)
  }

  extension (tuple: (Int, Int)) {
    def toRational = new Rational(tuple._1, tuple._2)
  }
}

/**
 * A class that represents a rational number.
 *
 * @param numerator The numerator of a rational number.
 * @param denominator The denominator of a rational number.
 */
class Rational(val numerator: Int, val denominator: Int):
  require(denominator != 0, "The denominator cannot be zero.")
  def this(numerator: Int) = this(numerator, 1)

  /**
   * Computes and returns the greatest common denominator.
   *
   * @param a The first number.
   * @param b The second number.
   * @return The greatest common denominator.
   */
  private def gcd(a: Int, b: Int): Int =
    if b <= 0 then a else gcd(b, a % b)

  /**
   * Adds two rational numbers.
   *
   * @param other The other rational number.
   * @return A rational number.
   */
  @targetName("add")
  def +(other: Rational): Rational =
    val denominator = this.denominator * other.denominator
    val numerator = (this.numerator * other.denominator) + (other.numerator * this.denominator)
    new Rational(numerator, denominator)

  /**
   * Subtracts two rational numbers.
   *
   * @param other The other rational number.
   * @return A rational number.
   */
  @targetName("subtract")
  def - (other: Rational): Rational =
    val denominator = this.denominator * other.denominator
    val numerator = (this.numerator * other.denominator) - (other.numerator * this.denominator)
    new Rational(numerator, denominator)

  /**
   * Divides two rational numbers.
   *
   * @param other The other rational number.
   * @return A rational number
   */
  @targetName("divide")
  def / (other: Rational): Rational =
    val numerator = this.numerator * other.flip.numerator
    val denominator = this.denominator * other.flip.denominator

    new Rational(numerator, denominator)

  /**
   * Multiplies two rational numbers.
   *
   * @param other The other rational number
   * @return A rational number.
   */
  @targetName("multiply")
  def * (other: Rational): Rational =
    val numerator = this.numerator * other.numerator
    val denominator = this.denominator * other.denominator
    new Rational(numerator, denominator)

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is less than the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("less_than_rational")
  def <(other: Rational): Boolean =
    this.toDouble < other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is less than the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("less_than_double")
  def <(other: Double): Boolean =
    this.toDouble < other

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is greater than the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("greater_than_rational")
  def >(other: Rational): Boolean =
    this.toDouble > other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is greater than the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("greater_than_double")
  def >(other: Double): Boolean =
    this.toDouble > other

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is less than or equal
   * to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("less_than_equal_rational")
  def <= (other: Rational): Boolean =
    this.toDouble <= other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is less than or equal
   * to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("less_than_equal_double")
  def <= (other: Double): Boolean =
    this.toDouble <= other

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is greater than or equal
   * to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("greater_than_equal_rational")
  def >= (other: Rational): Boolean =
    this.toDouble >= other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is less than or equal
   * to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("greater_than_equal_double")
  def >= (other: Double): Boolean =
    this.toDouble >= other

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is equal to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("equal_rational")
  def == (other: Rational): Boolean =
    this.toDouble == other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is equal to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("equal_double")
  def == (other: Double): Boolean =
    this.toDouble == other

  /**
   * Compares two rational numbers and determines
   * whether the left hand side is not equal to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("not_equal_rational")
  def != (other: Rational): Boolean =
    this.toDouble != other.toDouble

  /**
   * Compares a rational number and a double and determines
   * whether the left hand side is not equal to the right hand side.
   *
   * @param other The other rational number.
   * @return A boolean value.
   */
  @targetName("not_equal_double")
  def != (other: Double): Boolean =
    this.toDouble != other

  /**
   * Converts a rational number to a double.
   * @return A double value.
   */
  def toDouble: Double = numerator.toDouble / denominator.toDouble

  /**
   * Converts a rational number to a tuple.
   * @return A tuple value.
   */
  def toTuple: (Int, Int) = (numerator, denominator)

  /**
   * Flips the numerator with the denominator.
   * @return A rational number.
   */
  def flip: Rational = new Rational(denominator, numerator)

  /**
   * Reduces a rational number to its reduced form.
   *
   * For example, `10/20` is reduced to `1/2`.
   * @return A rational number.
   */
  def reduce: Rational =
    val common =  this.gcd(this.numerator.abs, this.denominator.abs)
    new Rational(numerator / common, denominator / common)

  /**
   * Computes and returns the greatest common denominator.
   * @return An integer value.
   */
  def gcd: Int = this.gcd(numerator, denominator)

  override def toString: String = s"$numerator/$denominator"

object Rational {
  def fromTuple(tuple: (Int, Int)) = new Rational(tuple._1, tuple._2)
}