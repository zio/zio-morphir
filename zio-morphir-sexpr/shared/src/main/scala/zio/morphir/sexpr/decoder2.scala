package zio.morphir.sexpr

import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.internal._
import zio.morphir.sexpr.javatime.parsers
import zio.morphir.sexpr.uuid.UUIDParser
import zio.{Chunk, NonEmptyChunk}
import zio.morphir.sexpr.SExprParser.*

import java.util.UUID
import scala.annotation._
import scala.collection.immutable.{LinearSeq, ListSet, TreeSet}
import scala.collection.{immutable, mutable}
import scala.util.control.NoStackTrace
import zio.parser.Parser

trait SExprDecoder2[A] {
  self =>

  final def decodeSExpr(str: CharSequence): Either[String, A] =
    decode(str.toString()) match {
      case Right(a)  => Right(a)
      case Left(err) => Left(err.render)
    }

  def fromAST(sexpr: SExpr): Either[String, A] =
    decodeSExpr(SExpr.encoder.encodeSExpr(sexpr, None))

  def decode(in: String): Either[SExprError, A]

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function.
   */
  def map[B](f: A => B): SExprDecoder2[B] = new SExprDecoder2[B] {
    def decode(in: String): Either[SExprError, B] =
      self.decode(in).map(f)

    override final def fromAST(sexpr: SExpr): Either[String, B] =
      self.fromAST(sexpr).map(f)
  }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may itself decide to fail
   * with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): SExprDecoder2[B] = new SExprDecoder2[B] {
    def decode(in: String): Either[SExprError, B] =
      self.decode(in).map(f) match {
        case Left(err)        => Left(err)
        case Right(Right(b))  => Right(b)
        case Right(Left(err)) => Left(SExprError.Message(err))
      }

    override final def fromAST(sexpr: SExpr): Either[String, B] = self.fromAST(sexpr).flatMap(f)
  }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics: such that if this
   * codec fails, the specified codec will be tried instead.
   */
  final def orElse[A1 >: A](that: => SExprDecoder2[A1]): SExprDecoder2[A1] = new SExprDecoder2[A1] {
    def decode(in: String): Either[SExprError, A1] =
      self.decode(in) match {
        case result @ Right(_) => result
        case Left(_)           => that.decode(in)
      }

    override final def fromAST(sexpr: SExpr): Either[String, A1] =
      self.fromAST(sexpr) match {
        case result @ Right(_) => result
        case Left(_)           => that.fromAST(sexpr)
      }
  }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics: such that if this
   * codec fails, the specified codec will be tried instead.
   */
  final def orElseEither[B](that: => SExprDecoder2[B]): SExprDecoder2[Either[A, B]] =
    self.map(Left(_)).orElse(that.map(Right(_)))

  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: SExprDecoder2[B] = self.asInstanceOf[SExprDecoder2[B]]

  /**
   * Returns a new codec that combines this codec and the specified codec into a single codec that decodes a tuple of
   * the values decoded by the respective codecs.
   */
  final def zip[B](that: => SExprDecoder2[B]): SExprDecoder2[(A, B)] = tuple2(this, that)

  /**
   * Zips two codecs, but discards the output on the right hand side.
   */
  final def zipLeft[B](that: => SExprDecoder2[B]): SExprDecoder2[A] = self.zipWith(that)((a, _) => a)

  /**
   * Zips two codecs, but discards the output on the left hand side.
   */
  final def zipRight[B](that: => SExprDecoder2[B]): SExprDecoder2[B] = self.zipWith(that)((_, b) => b)

  /**
   * Zips two codecs into one, transforming the outputs of both codecs by the specified function.
   */
  final def zipWith[B, C](that: => SExprDecoder2[B])(f: (A, B) => C): SExprDecoder2[C] =
    self.zip(that).map(f.tupled)

  // TODO
  implicit def tuple2[A1, A2](implicit A1: SExprDecoder2[A1], A2: SExprDecoder2[A2]): SExprDecoder2[Tuple2[A1, A2]] =
    new SExprDecoder2[Tuple2[A1, A2]] {
      def decode(in: String): Either[SExprError, (A1, A2)] = {
        // val a1 = A1.decode(in)
        // val a2 = A2.decode(in)
        // Tuple2(a1, a2)
        ???
      }
    }

  // def toError[A, B](a: Either[Parser.ParserError[String], A], f: A => B): Either[SExprError, B] =
  //   a match {
  //     case Right(a)  => Right(f(a))
  //     case Left(err) => Left(SExprError.ParseError(err.toString))
  //   }
}

object SExprDecoder2 {
  def apply[A](implicit decoder: SExprDecoder2[A]): SExprDecoder2[A] = decoder

  implicit val string: SExprDecoder2[String] = new SExprDecoder2[String] {
    override def decode(in: String): Either[SExprError, String] =
      SExprParser.grammar.str.parseString(in) match {
        case Right(a: SExpr.Str) => Right(a.value)
        case Left(err)           => Left(SExprError.ParseError(err.toString))
      }

    override final def fromAST(sexpr: SExpr): Either[String, String] =
      sexpr match {
        case SExpr.Str(value) => Right(value)
        case _                => Left("Not a string value")
      }
  }

  implicit val bool: SExprDecoder2[Boolean] = new SExprDecoder2[Boolean] {
    override def decode(in: String): Either[SExprError, Boolean] =
      SExprParser.grammar.bool.parseString(in) match {
        case Right(a)  => Right(a.value)
        case Left(err) => Left(SExprError.ParseError(err.toString))
      }

    override final def fromAST(sexpr: SExpr): Either[String, Boolean] =
      sexpr match {
        case SExpr.Bool(value) => Right(value)
        case _                 => Left("Not a bool value")
      }
  }

  // TODO: We may want to support Clojure style Character literals instead
  implicit val char: SExprDecoder2[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }

  implicit val bigInt: SExprDecoder2[java.math.BigInteger]     = fromBigInt(_.bigInteger, _.toBigInteger())
  implicit val scalaBigInt: SExprDecoder2[BigInt]              = fromBigInt(identity, _.toBigInteger())
  implicit val bigDecimal: SExprDecoder2[java.math.BigDecimal] = fromBigDecimal(_.bigDecimal, identity)
  implicit val scalaBigDecimal: SExprDecoder2[BigDecimal]      = fromBigDecimal(identity, BigDecimal.apply)
  implicit val byte: SExprDecoder2[Byte]                       = fromBigInt(_.toByte, _.byteValueExact())
  implicit val float: SExprDecoder2[Float]                     = fromBigDecimal(_.toFloat, _.floatValue())
  implicit val double: SExprDecoder2[Double]                   = fromBigDecimal(_.toDouble, _.doubleValue())
  implicit val int: SExprDecoder2[Int]                         = fromBigInt(_.toInt, _.intValueExact())
  implicit val long: SExprDecoder2[Long]                       = fromBigInt(_.toLong, _.longValueExact())
  implicit val short: SExprDecoder2[Short]                     = fromBigInt(_.toShort, _.shortValueExact())

  private[this] def fromBigInt[A](f: BigInt => A, fromBigDecimal: java.math.BigDecimal => A): SExprDecoder2[A] =
    new SExprDecoder2[A] {
      override def decode(in: String): Either[SExprError, A] =
        SExprParser.grammar.bigInt.parseString(in) match {
          case Right(a)  => Right(f(a))
          case Left(err) => Left(SExprError.ParseError(err.toString))
        }
      override final def fromAST(sexpr: SExpr): Either[String, A] =
        sexpr match {
          case SExpr.Num(value) => Right(fromBigDecimal(value))
          case _                => Left("Not a numeric value")
        }
    }

  private[this] def fromBigDecimal[A](f: BigDecimal => A, fromBigDecimal: java.math.BigDecimal => A): SExprDecoder2[A] =
    new SExprDecoder2[A] {
      override def decode(in: String): Either[SExprError, A] =
        SExprParser.grammar.num.parseString(in) match {
          case Right(a)  => Right(f(a.value))
          case Left(err) => Left(SExprError.ParseError(err.toString))
        }
      override final def fromAST(sexpr: SExpr): Either[String, A] =
        sexpr match {
          case SExpr.Num(value) => Right(fromBigDecimal(value))
          case _                => Left("Not a numeric value")
        }
    }
}
