package de.heisluft.rationals

import scala.language.implicitConversions

abstract class Rational(private var numerator: Int, private var denominator: Int) extends Comparable[Rational] {
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

  if(denominator == 0) throw new IllegalArgumentException("division by 0")

  private def gcd = Math.gcd(numerator, denominator)

  final def num: Int = numerator / gcd
  final def denom: Int = denominator / gcd

  protected def setNum(to: Int): Unit = numerator = to

  protected def setDenom(to: Int): Unit = denominator = to

  def this(numerator: Int) = this(numerator, 1)

  override final def compareTo(other: Rational): Int = if(this == other) 0 else if(this < other) -1 else 1

  override final def toString: String = num + "/" + denom

  final def <(other: Rational): Boolean = (this - other).num < 0

  final def <(other: Int): Boolean = this - other < Rational.ZERO

  final def >(other: Rational): Boolean = (this - other).num > 0

  final def >(other: Int): Boolean = this - other > Rational.ZERO

  final def <=(other: Rational): Boolean = this == other || this < other

  final def <=(other: Int): Boolean = this - other <= Rational.ZERO

  final def >=(other: Rational): Boolean = this == other || this > other

  final def >=(other: Int): Boolean = this - other >= Rational.ZERO

  final def ==(other: Rational): Boolean = denom == other.denom && num == other.num

  final def ==(other: Int): Boolean = this == Rational.int2Rat(other)

  final def toDouble: Double = num.toDouble / denom.toDouble

  def -(other: Rational): Rational

  def -(other: Int): Rational

  def +(other: Rational): Rational

  def +(other: Int): Rational

  def *(other: Rational): Rational

  def *(other: Int): Rational

  def /(other: Rational): Rational

  def /(other: Int): Rational


}

object Rational {
  implicit def int2Rat(toConvert: Int): Rational = new ImmutableRational(toConvert, 1)

  final val ZERO: Rational = 0
}