package zio.morphir.sexpr.ast

import zio.Chunk
import zio.morphir.sexpr.{SExprDecoder, SExprEncoder, SExprError}
import zio.morphir.sexpr.SExprDecoder._
import zio.morphir.sexpr.internal._

sealed trait SExpr { self =>
  import SExprCase._
  def $case: SExprCase[SExpr]

  def fold[Z](f: SExprCase[Z] => Z): Z = self.$case match {
    case c @ BoolCase(_)       => f(c)
    case c @ StrCase(_)        => f(c)
    case c @ NumCase(_)        => f(c)
    case c @ SymbolCase(_)     => f(c)
    case c @ KeyWordCase(_, _) => f(c)
    case MapCase(items)        => f(MapCase(items.map { case (k, v) => (k.fold(f), v.fold(f)) }))
    case NilCase               => f(NilCase)
    case ConsCase(head, tail)  => f(ConsCase(head.fold(f), tail.fold(f)))
    case QuotedCase(get)       => f(QuotedCase(get.fold(f)))
    case VectorCase(items)     => f(VectorCase(items.map(_.fold(f))))
  }

  final def widen: SExpr = this
}

object SExpr {
  import SExprCase._

  implicit val decoder: SExprDecoder[SExpr] = new SExprDecoder[SExpr] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): SExpr = {
      val c = in.nextNonWhitespace()
      in.retract()
      c match {
        case 'n'             => Nil.decoder.unsafeDecode(trace, in)
        case 'f' | 't'       => Bool.decoder.unsafeDecode(trace, in)
        case '['             => SVector.decoder.unsafeDecode(trace, in)
        case '"'             => Str.decoder.unsafeDecode(trace, in)
        case ':'             => KeyWord.decoder.unsafeDecode(trace, in)
        case '#' | '/' | '.' => Symbol.decoder.unsafeDecode(trace, in)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          Num.decoder.unsafeDecode(trace, in)
        case c =>
          throw UnsafeSExpr(SExprError.Message(s"unexpected '$c'") :: trace)
      }
    }

    override final def fromAST(sexpr: SExpr): Either[String, SExpr] = Right(sexpr)
  }

  implicit val encoder: SExprEncoder[SExpr] = new SExprEncoder[SExpr] {
    def unsafeEncode(a: SExpr, indent: Option[Int], out: Write): Unit =
      a match {
        case j: SVector => SVector.encoder.unsafeEncode(j, indent, out)
        case j: SMap    => SMap.encoder.unsafeEncode(j, indent, out)
        case j: KeyWord => KeyWord.encoder.unsafeEncode(j, indent, out)
        case j: Symbol  => Symbol.encoder.unsafeEncode(j, indent, out)
        case j: Bool    => Bool.encoder.unsafeEncode(j, indent, out)
        case j: Str     => Str.encoder.unsafeEncode(j, indent, out)
        case j: Num     => Num.encoder.unsafeEncode(j, indent, out)
        case Nil        => Nil.encoder.unsafeEncode(Nil, indent, out)
      }

    override final def toAST(a: SExpr): Either[String, SExpr] = Right(a)
  }

  def bool(value: Boolean): Bool = Bool(value)

  def vector(items: SExpr*): SVector = SVector(Chunk(items: _*))

  final case class Bool private[ast] ($case: BoolCase) extends SExpr
  object Bool {
    def apply(value: Boolean): Bool = Bool(BoolCase(value))
    def unapply(arg: SExpr): Option[Boolean] = arg.$case match {
      case BoolCase(value) => Some(value)
      case _               => None
    }

    val False: Bool = Bool(false)
    val True: Bool  = Bool(true)

    implicit val decoder: SExprDecoder[Bool] = new SExprDecoder[Bool] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): Bool =
        Bool(SExprDecoder.boolean.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, Bool] =
        sexpr match {
          case b: Bool => Right(b)
          case _       => Left(s"Not a bool value")
        }
    }

    implicit val encoder: SExprEncoder[Bool] = new SExprEncoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: Write): Unit =
        SExprEncoder.boolean.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Bool): Either[String, SExpr] = Right(a)
    }
  }

  final case class SMap private[ast] ($case: MapCase[SExpr]) extends SExpr
  object SMap {
    def apply(items: Map[SExpr, SExpr]): SMap = SMap(MapCase(items))
    def unapply(arg: SExpr): Option[Map[SExpr, SExpr]] = arg.$case match {
      case MapCase(items: Map[SExpr, SExpr]) => Some(items)
      case _                                 => None
    }

    implicit val decoder: SExprDecoder[SMap] = new SExprDecoder[SMap] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): SMap = ??? // TO DO
      // SMap(SExprDecoder.map.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, SMap] =
        sexpr match {
          case k: SMap => Right(k)
          case _       => Left(s"Not a map")
        }
    }

    implicit val encoder: SExprEncoder[SMap] = new SExprEncoder[SMap] {
      def unsafeEncode(a: SMap, indent: Option[Int], out: Write): Unit = ??? // TO DO
      // SExprEncoder.map.unsafeEncode(a.$case.items, indent, out)

      override final def toAST(a: SMap): Either[String, SMap] = Right(a)
    }
  }

  final case class Symbol private[ast] ($case: SymbolCase) extends SExpr
  object Symbol {
    def apply(value: String): Symbol = Symbol(SymbolCase(value))
    def unapply(arg: SExpr): Option[String] = arg.$case match {
      case SymbolCase(value) => Some(value)
      case _                 => None
    }

    implicit val decoder: SExprDecoder[Symbol] = new SExprDecoder[Symbol] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): Symbol = ??? // TODO
      // Symbol(SExprDecoder.symbol.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, Symbol] =
        sexpr match {
          case k: Symbol => Right(k)
          case _         => Left(s"Not a symbol")
        }
    }

    implicit val encoder: SExprEncoder[Symbol] = new SExprEncoder[Symbol] {
      def unsafeEncode(a: Symbol, indent: Option[Int], out: Write): Unit = ??? // TODO
      // SExprEncoder.symbol.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Symbol): Either[String, Symbol] = Right(a)
    }
  }

  final case class KeyWord private[ast] ($case: KeyWordCase) extends SExpr
  object KeyWord {
    def apply(value: String, isMacro: Boolean): KeyWord = KeyWord(KeyWordCase(value, isMacro))
    def unapply(arg: SExpr): Option[(String, Boolean)] = arg.$case match {
      case KeyWordCase(value, isMacro) => Some(value -> isMacro)
      case _                           => None
    }

    implicit val decoder: SExprDecoder[KeyWord] = new SExprDecoder[KeyWord] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): KeyWord = ??? // TODO
      // KeyWord(SExprDecoder.keyword.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, KeyWord] =
        sexpr match {
          case k: KeyWord => Right(k)
          case _          => Left(s"Not a keyword")
        }
    }

    implicit val encoder: SExprEncoder[KeyWord] = new SExprEncoder[KeyWord] {
      def unsafeEncode(a: KeyWord, indent: Option[Int], out: Write): Unit = ??? // TODO
      // SExprEncoder.keyword.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: KeyWord): Either[String, KeyWord] = Right(a)
    }
  }

  case object Nil extends SExpr {
    val $case = NilCase

    private[this] val nilChars: Array[Char] = "nil".toCharArray
    implicit val decoder: SExprDecoder[Nil.type] = new SExprDecoder[Nil.type] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): Nil.type = {
        Lexer.readChars(trace, in, nilChars, "nil")
        Nil
      }

      override final def fromAST(sexpr: SExpr): Either[String, Nil.type] =
        sexpr match {
          case Nil => Right(Nil)
          case _   => Left(s"Not nil")
        }
    }
    implicit val encoder: SExprEncoder[Nil.type] = new SExprEncoder[Nil.type] {
      def unsafeEncode(a: Nil.type, indent: Option[Int], out: Write): Unit =
        out.write("nil")

      override final def toAST(a: Nil.type): Either[String, SExpr] = Right(a)
    }
  }

  final case class Num private[ast] ($case: NumCase) extends SExpr
  object Num {
    def apply(value: java.math.BigDecimal): Num = Num(NumCase(value))
    def apply(value: BigDecimal): Num           = Num(value.bigDecimal)
    def apply(value: Byte): Num                 = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Double): Num               = Num(BigDecimal(value).bigDecimal)
    def apply(value: Float): Num                = Num(BigDecimal(value.toDouble).bigDecimal)
    def apply(value: Int): Num                  = Num(BigDecimal(value).bigDecimal)
    def apply(value: Long): Num                 = Num(BigDecimal(value).bigDecimal)
    def apply(value: Short): Num                = Num(BigDecimal(value.toInt).bigDecimal)

    def unapply(exp: SExpr): Option[java.math.BigDecimal] = exp.$case match {
      case NumCase(value) => Some(value)
      case _              => None
    }

    implicit val decoder: SExprDecoder[Num] = new SExprDecoder[Num] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): Num =
        Num(SExprDecoder.bigDecimal.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, Num] =
        sexpr match {
          case n: Num => Right(n)
          case _      => Left(s"Not a number")
        }
    }

    implicit val encoder: SExprEncoder[Num] = new SExprEncoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: Write): Unit =
        SExprEncoder.bigDecimal.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Num): Either[String, Num] = Right(a)
    }
  }

  final case class Str private[ast] ($case: StrCase) extends SExpr
  object Str {
    def apply(value: String): Str = Str(StrCase(value))
    def unapply(exp: SExpr): Option[String] = exp.$case match {
      case StrCase(value) => Some(value)
      case _              => None
    }

    implicit val decoder: SExprDecoder[Str] = new SExprDecoder[Str] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): Str =
        Str(SExprDecoder.string.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, Str] =
        sexpr match {
          case s: Str => Right(s)
          case _      => Left(s"Not a string value")
        }
    }
    implicit val encoder: SExprEncoder[Str] = new SExprEncoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: Write): Unit =
        SExprEncoder.string.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Str): Either[String, SExpr] = Right(a)
    }
  }

  final case class SVector private[ast] ($case: VectorCase[SExpr]) extends SExpr
  object SVector {
    def apply(items: Chunk[SExpr]): SVector = SVector(VectorCase(items))
    def unapply(arg: SExpr): Option[Chunk[SExpr]] = arg.$case match {
      case VectorCase(items) => Some(items)
      case _                 => None
    }

    private lazy val vectD = SExprDecoder.chunk[SExpr]
    implicit val decoder: SExprDecoder[SVector] = new SExprDecoder[SVector] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): SVector =
        SVector(vectD.unsafeDecode(trace, in))

      override final def fromAST(SExpr: SExpr): Either[String, SVector] =
        SExpr match {
          case vect: SVector => Right(vect)
          case _             => Left(s"Not a vector")
        }
    }

    private lazy val vactE = SExprEncoder.chunk[SExpr]
    implicit val encoder: SExprEncoder[SVector] = new SExprEncoder[SVector] {
      def unsafeEncode(a: SVector, indent: Option[Int], out: Write): Unit =
        vactE.unsafeEncode(a.$case.items, indent, out)

      override final def toAST(a: SVector): Either[String, SExpr] = Right(a)
    }
  }
}

sealed trait SExprCase[+Self] { self =>
  import SExprCase._

  def map[B](f: Self => B): SExprCase[B] = self match {
    case BoolCase(value)             => BoolCase(value)
    case ConsCase(head, tail)        => ConsCase(f(head), f(tail))
    case StrCase(value)              => StrCase(value)
    case SymbolCase(value)           => SymbolCase(value)
    case KeyWordCase(value, isMacro) => KeyWordCase(value, isMacro)
    case MapCase(items)              => MapCase(items.map { case (k, v) => (f(k), f(v)) })
    case NilCase                     => NilCase
    case NumCase(value)              => NumCase(value)
    case QuotedCase(get)             => QuotedCase(f(get))
    case VectorCase(items)           => VectorCase(items.map(f))
  }
}

object SExprCase {
  sealed trait AtomCase[+Self]       extends SExprCase[Self]
  sealed trait CollectionCase[+Self] extends SExprCase[Self]
  sealed trait ListCase[+Self]       extends CollectionCase[Self]
  sealed trait SymbolBaseCase[+Self] extends AtomCase[Self]

  // Leaf Cases
  final case class BoolCase(value: Boolean)                     extends SymbolBaseCase[Nothing]
  final case class KeyWordCase(value: String, isMacro: Boolean) extends AtomCase[Nothing]
  case object NilCase                                           extends ListCase[Nothing]
  final case class NumCase(value: java.math.BigDecimal)         extends AtomCase[Nothing]
  final case class SymbolCase(value: String)                    extends SymbolBaseCase[Nothing]
  final case class StrCase(value: String)                       extends AtomCase[Nothing]

  // Recursive Cases
  final case class ConsCase[+Self](head: Self, tail: Self) extends ListCase[Self]
  final case class MapCase[Self](items: Map[Self, Self])   extends CollectionCase[Self]
  final case class QuotedCase[+Self](get: Self)            extends SExprCase[Self]
  final case class VectorCase[+Self](items: Chunk[Self])   extends CollectionCase[Self]
}
