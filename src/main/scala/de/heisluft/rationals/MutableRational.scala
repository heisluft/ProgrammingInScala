package de.heisluft.rationals

// FIXME: Implement properly
class MutableRational(numerator: Int, denominator: Int) extends Rational(numerator, denominator) {

  override def *(other: Rational): Rational = {
    setNum(num * other.num)
    setDenom(denom * other.denom)
    this
  }

  override def *(other: Int): Rational = this

  override def /(other: Rational): Rational = this

  override def /(other: Int): Rational = this

  override def +(other: Rational): Rational = this

  override def +(other: Int): Rational = this

  override def -(other: Rational): Rational = this

  override def -(other: Int): Rational = this
}