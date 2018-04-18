package de.heisluft.scalaserver

class Rational(val num: Int, val denom: Int) extends Comparable[Rational] {

  object Math {

    /**
      * Return the greatest common divisor of two Integers a and b
      *
      * @param a the first Integer
      * @param b the second Integer
      * @return the greatest common divisor
      */
    def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  }

  if(denom == 0) throw new IllegalArgumentException("division by 0")

  override def compareTo(other: Rational): Int = if(this == other) 0 else if(this < other) -1 else 1

  override def toString: String = num + "/" + denom

  def /(other: Rational): Rational = new Rational(num * other.denom, denom * other.num).toIrreducible

  def /(other: Int): Rational = {
    if(num % other == 0) new Rational(num / other, denom).toIrreducible
    new Rational(num, denom * other).toIrreducible
  }

  def *(other: Rational):Rational = new Rational(num * other.num, denom * other.denom).toIrreducible

  def *(other: Int): Rational = new Rational(num * other, denom).toIrreducible

  def -(other: Rational): Rational = {
    if(other.denom == denom) new Rational(num - other.num, denom).toIrreducible
    new Rational(num * other.denom - other.num * denom, denom * other.denom).toIrreducible
  }

  def -(other: Int): Rational = new Rational(num - other * denom, denom).toIrreducible

  def <(other: Rational): Boolean = (this - other).num < 0

  def <(other: Int): Boolean = this - other < Rational.ZERO

  def >(other: Rational): Boolean = (this - other).num > 0

  def >(other: Int): Boolean = this - other > Rational.ZERO

  def <=(other: Rational): Boolean = this == other || this < other

  def <=(other: Int): Boolean = this - other <= Rational.ZERO

  def >=(other: Rational): Boolean = this == other || this > other

  def >=(other: Int): Boolean = this - other >= Rational.ZERO

  def ==(other: Rational): Boolean = {
    val i1 = toIrreducible
    val i2 = other.toIrreducible
    i1.denom == i2.denom && i1.num == i2.num
  }

  def ==(other: Int): Boolean = this == Rational.int2Rat(other)

  def +(other: Rational): Rational = {
    if(other.denom == denom) new Rational(num + other.num, denom)
    new Rational(num * other.denom + other.num * denom, denom * other.denom).toIrreducible
  }

  def +(other: Int): Rational = new Rational(num + other * denom, denom).toIrreducible

  def toIrreducible: Rational = {
    val gcd = Math.gcd(num, denom)
    if(gcd == 1) this else new Rational(num / gcd, denom / gcd)
  }
}

object Rational {
  implicit def int2Rat(toConvert: Int): Rational = new Rational(toConvert, 1)

  final val ZERO: Rational = 0
}