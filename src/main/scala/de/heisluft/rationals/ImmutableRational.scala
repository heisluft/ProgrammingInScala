package de.heisluft.rationals

class ImmutableRational(numerator: Int, denominator: Int) extends Rational(numerator, denominator) {

  override def /(other: Rational): Rational = new ImmutableRational(num * other.denom, denom * other.num)

  override def /(other: Int): Rational = {
    if(num % other == 0) new ImmutableRational(num / other, denom)
    new ImmutableRational(num, denom * other)
  }

  override protected def setDenom(to: Int): Unit = throw new UnsupportedOperationException

  override def setNum(to: Int): Unit = throw new UnsupportedOperationException

  override def *(other: Rational): Rational = new ImmutableRational(num * other.num, denom * other.denom)

  override def *(other: Int): Rational = new ImmutableRational(num * other, denom)

  override def -(other: Rational): Rational = {
    if(other.denom == denom) new ImmutableRational(num - other.num, denom)
    new ImmutableRational(num * other.denom - other.num * denom, denom * other.denom)
  }

  override def -(other: Int): Rational = new ImmutableRational(num - other * denom, denom)
  
  override def +(other: Rational): Rational = {
    if(other.denom == denom) new ImmutableRational(num + other.num, denom)
    new ImmutableRational(num * other.denom + other.num * denom, denom * other.denom)
  }

  override def +(other: Int): Rational = new ImmutableRational(num + other * denom, denom)

}