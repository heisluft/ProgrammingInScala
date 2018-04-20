package de.heisluft.rationals

class MutableRational(numerator: Int, denominator: Int) extends Rational(numerator, denominator) {

  def this(numerator: Int) = this(numerator, 1)

  override def *(other: Rational): Rational = {
    setNum(num * other.num)
    setDenom(denom * other.denom)
    this
  }

  override def *(other: Int): Rational = {
    setNum(num * other)
    this
  }

  override def /(other: Rational): Rational = {
    setNum(num * other.denom)
    setDenom(denom * other.num)
    this
  }

  override def /(other: Int): Rational = {
    setDenom(denom * other)
    this
  }

  override def +(other: Rational): Rational = {
    if(denom == other.denom) setNum(num + other.num)
    else {
      setNum(num * other.denom + other.num * denom)
      setDenom(denom * other.denom)
    }
    this
  }

  override def +(other: Int): Rational = {
    setNum(num + other * denom)
    this
  }

  override def -(other: Rational): Rational = {
    if(denom == other.denom) setNum(num - other.num)
    else {
      setNum(num * other.denom - other.num * denom)
      setDenom(denom * other.denom)
    }
    this
  }

  override def -(other: Int): Rational = {
    setNum(num - other * denom)
    this
  }

  def toImmutable: Rational = new ImmutableRational(num, denom)
}