package zio.morphir.ir
import zio.morphir.ir.Type.UType
import zio.morphir.ir.Value.{RawValue, TypedValue}

import scala.language.implicitConversions

sealed trait Literal[+A] { self =>
  def value: A
  final def toRawValue: RawValue = Value.literal(self)
  final def inferredType: UType  = InferredTypeOf[Literal[A]].inferredType(self)
  final def toTypedValue[A0 >: A](implicit ev: InferredTypeOf[Literal[A0]]): TypedValue = {
    val tpe = ev.inferredType(self)
    Value.literal(tpe, self)
  }

  final override def toString: java.lang.String = Literal.toString(self)
}

object Literal {
  def boolean(value: Boolean): Bool                         = Bool(value)
  def char(value: scala.Char): Char                         = Char(value)
  def decimal(value: java.math.BigDecimal): Float           = Float(value)
  def decimal(value: BigDecimal): Float                     = Float(value.bigDecimal)
  def double(value: scala.Double): Float                    = Float(java.math.BigDecimal.valueOf(value))
  def float(value: scala.Float): Float                      = Float(java.math.BigDecimal.valueOf(value.toDouble))
  def int(value: Int): WholeNumber                          = WholeNumber(java.math.BigInteger.valueOf(value.toLong))
  def long(value: Long): WholeNumber                        = WholeNumber(java.math.BigInteger.valueOf(value))
  def string(value: java.lang.String): String               = Literal.String(value)
  def wholeNumber(value: java.math.BigInteger): WholeNumber = WholeNumber(value)
  def wholeNumber(value: scala.BigInt): WholeNumber         = WholeNumber(value.bigInteger)

  val False: Bool = Bool(false)
  val True: Bool  = boolean(true)

  implicit def literalToRawValue[A](literal: Literal[A]): RawValue = literal.toRawValue

  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  final case class Float(value: java.math.BigDecimal)       extends Literal[java.math.BigDecimal]

  def toString(literal: Literal[_]): java.lang.String = literal match {
    case Bool(true)         => "True"
    case Bool(false)        => "False"
    case Char(value)        => s"'$value'"
    case String(value)      => s""""$value""""
    case WholeNumber(value) => value.toString
    case Float(value)       => value.toString

  }

  implicit def LiteralInferredTypeOf[A]: InferredTypeOf[Literal[A]] = new InferredTypeOf[Literal[A]] {
    def inferredType(value: Literal[A]): UType = value match {
      case Bool(_)        => sdk.Basics.boolType
      case Char(_)        => sdk.Char.charType
      case String(_)      => sdk.String.stringType
      case WholeNumber(_) => sdk.Basics.intType
      case Float(_)       => sdk.Basics.floatType
    }
  }
}
