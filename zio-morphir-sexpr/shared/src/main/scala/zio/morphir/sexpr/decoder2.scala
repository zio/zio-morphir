package zio.morphir.sexpr

import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.javatime.parsers
import zio.morphir.sexpr.uuid.UUIDParser
import zio.{Chunk, NonEmptyChunk}

import java.util.UUID
import scala.collection.immutable.*

trait SExprDecoder2[A] {
  self =>

  /**
   * An alias for [[SExprDecoder2#orElse]].
   */
  final def <>[A1 >: A](that: => SExprDecoder2[A1]): SExprDecoder2[A1] = self.orElse(that)

  /**
   * An alias for [[SExprDecoder2#orElseEither]].
   */
  final def <+>[B](that: => SExprDecoder2[B]): SExprDecoder2[Either[A, B]] = self.orElseEither(that)

  /**
   * An alias for [[SExprDecoder2#zip]].
   */
  final def <*>[B](that: => SExprDecoder2[B]): SExprDecoder2[(A, B)] = self.zip(that)

  /**
   * An alias for [[SExprDecoder2#zipRight]].
   */
  final def *>[B](that: => SExprDecoder2[B]): SExprDecoder2[B] = self.zipRight(that)

  /**
   * An alias for [[SExprDecoder2#zipLeft]].
   */
  final def <*[B](that: => SExprDecoder2[B]): SExprDecoder2[A] = self.zipLeft(that)

  /**
   * Attempts to decode a value of type `A` from the specified `CharSequence`, but may fail with a human-readable error
   * message if the provided text does not encode a value of this type.
   *
   * Note: This method may not entirely consume the specified character sequence.
   */
  final def decodeSExpr(str: CharSequence): Either[String, A] =
    decode(str.toString()) match {
      case Right(a)  => Right(a)
      case Left(err) => Left(err.render)
    }

  /**
   * Decode a value from an already parsed SExpr AST.
   *
   * The default implementation encodes the SExpr to a byte stream and uses decode to parse that. Override to provide a
   * more performant implementation.
   */
  def fromAST(sexpr: SExpr): Either[String, A] =
    decodeSExpr(SExpr.encoder.encodeSExpr(sexpr, None))

  /**
   * Low-level, method to decode a value or return an exception. This method should not be called in application code,
   * although it can be implemented for user-defined data structures.
   */
  def decode(in: String): Either[SExprError, A]

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function.
   */
  def map[B](f: A => B): SExprDecoder2[B] = new SExprDecoder2[B] {
    override def decode(in: String): Either[SExprError, B] =
      self.decode(in).map(f)

    override final def fromAST(sexpr: SExpr): Either[String, B] =
      self.fromAST(sexpr).map(f)
  }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may itself decide to fail
   * with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): SExprDecoder2[B] = new SExprDecoder2[B] {
    override def decode(in: String): Either[SExprError, B] =
      self.decode(in).map(f) match {
        case Left(err)        => Left(err)
        case Right(Right(b))  => Right(b)
        case Right(Left(err)) => Left(SExprError.Message(err))
      }

    override final def fromAST(sexpr: SExpr): Either[String, B] =
      self.fromAST(sexpr).flatMap(f)
  }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics: such that if this
   * codec fails, the specified codec will be tried instead.
   */
  final def orElse[A1 >: A](that: => SExprDecoder2[A1]): SExprDecoder2[A1] = new SExprDecoder2[A1] {
    override def decode(in: String): Either[SExprError, A1] =
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

  implicit def tuple2[A1, A2](implicit A1: SExprDecoder2[A1], A2: SExprDecoder2[A2]): SExprDecoder2[(A1, A2)] =
    new SExprDecoder2[Tuple2[A1, A2]] {
      override def decode(in: String): Either[SExprError, (A1, A2)] = {
        for {
          a1 <- A1.decode(in)
          a2 <- A2.decode(in)
        } yield (a1, a2)
      }
    }
}

object SExprDecoder2 {
  def apply[A](implicit decoder: SExprDecoder2[A]): SExprDecoder2[A] = decoder

  implicit val string: SExprDecoder2[String] = new SExprDecoder2[String] {
    override def decode(in: String): Either[SExprError, String] =
      SExprParser.grammar.str.parseString(in) match {
        case Right(a: SExpr.Str) =>
          new zio.morphir.sexpr.internal.StringEscape(a.value).run match {
            case Left(err)  => Left(SExprError.ParseError(err))
            case Right(str) => Right(str)
          }
        case Left(err) => Left(SExprError.ParseError(err.toString))
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
  // TODO Need to add support for escape characters
  implicit val char: SExprDecoder2[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case str                    => Left(s"expected one character; but got size ${str.length} char=[$str]")
    // case _                      => Left("expected one character")
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

  // Option treats empty and nil values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass nil to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit a: SExprDecoder2[A]): SExprDecoder2[Option[A]] = new SExprDecoder2[Option[A]] {
    self =>
    override def decode(in: String): Either[SExprError, Option[A]] =
      SExprParser.grammar.nil.parseString(in) match {
        case Right(SExpr.Nil) => Right(None)
        case _                => a.decode(in).map(Some.apply)
      }

    override final def fromAST(sexpr: SExpr): Either[String, Option[A]] = sexpr match {
      case SExpr.Nil => Right(None)
      case _         => a.fromAST(sexpr).map(Some.apply)
    }

    // overridden here to pass `None` to the new Decoder instead of throwing
    // when called from a derived decoder
    override def map[B](f: Option[A] => B): SExprDecoder2[B] = new SExprDecoder2[B] {
      override def decode(in: String): Either[SExprError, B] =
        self.decode(in).map(f)

      override final def fromAST(sexpr: SExpr): Either[String, B] =
        self.fromAST(sexpr).map(f)
    }
  }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](implicit A: SExprDecoder2[A], B: SExprDecoder2[B]): SExprDecoder2[Either[A, B]] =
    new SExprDecoder2[Either[A, B]] { self =>
      lazy val leftSymbols  = List("a", "Left", "left").map(SExpr.Symbol.apply)
      lazy val rightSymbols = List("b", "Right", "right").map(SExpr.Symbol.apply)

      override def decode(in: String): Either[SExprError, Either[A, B]] =
        SExprParser.grammar.map.parseString(in) match {
          case Left(err)                             => Left(SExprError.ParseError(err.toString))
          case Right(smap: SExpr.SMap[SExpr, SExpr]) => self.fromAST(smap).left.map(SExprError.ParseError(_))
        }

      override final def fromAST(sexpr: SExpr): Either[String, Either[A, B]] = sexpr match {
        case SExpr.SMap(m: Map[SExpr, SExpr]) if (m.size == 1 && leftSymbols.exists(m.contains)) =>
          A.fromAST(m.values.head) match {
            case Left(err) => Left(err)
            case Right(a)  => Right(Left(a))
          }
        case SExpr.SMap(m: Map[SExpr, SExpr]) if (m.size == 1 && rightSymbols.exists(m.contains)) =>
          B.fromAST(m.values.head) match {
            case Left(err) => Left(err)
            case Right(b)  => Right(Right(b))
          }
        case _ => Left("Not an either")
      }
    }

  implicit def immutableMap[K, V](implicit K: SExprDecoder2[K], V: SExprDecoder2[V]): SExprDecoder2[Map[K, V]] =
    new SExprDecoder2[Map[K, V]] { self =>
      override def decode(in: String): Either[SExprError, Map[K, V]] =
        SExprParser.grammar.map.parseString(in) match {
          case Left(err)                             => Left(SExprError.ParseError(err.toString))
          case Right(smap: SExpr.SMap[SExpr, SExpr]) => self.fromAST(smap).left.map(SExprError.ParseError(_))
        }

      override final def fromAST(sexpr: SExpr): Either[String, Map[K, V]] = sexpr match {
        case SExpr.SMap(m: Map[SExpr, SExpr]) =>
          m.foldLeft[Either[String, Map[K, V]]](Right(Map.empty)) { case (s, (k, v)) =>
            for {
              map   <- s
              key   <- K.fromAST(k)
              value <- V.fromAST(v)
            } yield map + (key -> value)
          }
        case _ => Left("Not a map")
      }
    }

  implicit def chunk[A: SExprDecoder2]: SExprDecoder2[Chunk[A]] = new SExprDecoder2[Chunk[A]] {
    override def decode(in: String): Either[SExprError, Chunk[A]] =
      SExprParser.grammar.vector.parseString(in) match {
        case Right(SExpr.SVector(elements)) =>
          elements.foldLeft[Either[SExprError, Chunk[A]]](Right(Chunk.empty)) { (s, item) =>
            s.flatMap(chunk =>
              implicitly[SExprDecoder2[A]].fromAST(item) match {
                case Right(a)  => Right(chunk :+ a)
                case Left(err) => Left(SExprError.Message(err))
              }
            )
          }
        case Left(err) => Left(SExprError.ParseError(err.toString))
      }

    override final def fromAST(sexpr: SExpr): Either[String, Chunk[A]] =
      sexpr match {
        case SExpr.SVector(elements) =>
          elements.foldLeft[Either[String, Chunk[A]]](Right(Chunk.empty)) { (s, item) =>
            s.flatMap(chunk =>
              implicitly[SExprDecoder2[A]].fromAST(item).map { a =>
                chunk :+ a
              }
            )
          }
        case _ => Left("Not an array")
      }
  }

  implicit def nonEmptyChunk[A: SExprDecoder2]: SExprDecoder2[NonEmptyChunk[A]] =
    chunk[A].mapOrFail(NonEmptyChunk.fromChunk(_).toRight("Chunk was empty"))

  implicit def array[A: SExprDecoder2: reflect.ClassTag]: SExprDecoder2[Array[A]] = chunk[A].map(_.toArray)
  implicit def list[A: SExprDecoder2]: SExprDecoder2[List[A]]                     = chunk[A].map(_.toList)
  implicit def seq[A: SExprDecoder2]: SExprDecoder2[Seq[A]]                       = chunk[A].map(identity)
  implicit def indexedSeq[A: SExprDecoder2]: SExprDecoder2[IndexedSeq[A]]         = chunk[A].map(identity)
  implicit def linearSeq[A: SExprDecoder2]: SExprDecoder2[LinearSeq[A]]           = chunk[A].map(_.toList)
  implicit def vector[A: SExprDecoder2]: SExprDecoder2[Vector[A]]                 = chunk[A].map(_.toVector)
  implicit def set[A: SExprDecoder2]: SExprDecoder2[Set[A]]                       = chunk[A].map(_.toSet)
  implicit def hashSet[A: SExprDecoder2]: SExprDecoder2[HashSet[A]]               = chunk[A].map(new HashSet() ++ _)
  implicit def listSet[A: SExprDecoder2]: SExprDecoder2[ListSet[A]]               = chunk[A].map(new ListSet() ++ _)
  implicit def sortedSet[A: SExprDecoder2: Ordering]: SExprDecoder2[SortedSet[A]] = chunk[A].map(new TreeSet() ++ _)
  implicit def treeSet[A: SExprDecoder2: Ordering]: SExprDecoder2[TreeSet[A]]     = chunk[A].map(new TreeSet() ++ _)
  implicit def iterable[A: SExprDecoder2]: SExprDecoder2[Iterable[A]]             = chunk[A].map(identity)

  implicit def hashMap[K: SExprDecoder2, V: SExprDecoder2]: SExprDecoder2[HashMap[K, V]] =
    immutableMap[K, V].map(new HashMap() ++ _)
  implicit def mutableMap[K: SExprDecoder2, V: SExprDecoder2]: SExprDecoder2[collection.mutable.Map[K, V]] =
    immutableMap[K, V].map(new collection.mutable.HashMap() ++ _)
  implicit def mutableHashMap[K: SExprDecoder2, V: SExprDecoder2]: SExprDecoder2[collection.mutable.HashMap[K, V]] =
    immutableMap[K, V].map(new collection.mutable.HashMap() ++ _)
  implicit def sortedMap[K: SExprDecoder2: Ordering, V: SExprDecoder2]: SExprDecoder2[collection.SortedMap[K, V]] =
    treeMap[K, V].map(new TreeMap() ++ _)
  implicit def treeMap[K: SExprDecoder2: Ordering, V: SExprDecoder2]: SExprDecoder2[TreeMap[K, V]] =
    immutableMap[K, V].map(new TreeMap() ++ _)

  // use this instead of `string.mapOrFail` in supertypes (to prevent class initialization error at runtime)
  private[sexpr] def mapString[A](f: String => Either[String, A]): SExprDecoder2[A] =
    new SExprDecoder2[A] {
      override def decode(in: String): Either[SExprError, A] =
        string.decode(in).map(f) match {
          case Left(err)        => Left(err)
          case Right(Right(b))  => Right(b)
          case Right(Left(err)) => Left(SExprError.Message(err))
        }

      override def fromAST(sexpr: SExpr): Either[String, A] =
        string.fromAST(sexpr).flatMap(f)
    }

  import java.time.format.DateTimeParseException
  import java.time.zone.ZoneRulesException
  import java.time._

  implicit val dayOfWeek: SExprDecoder2[DayOfWeek] =
    mapString(s => parseJavaTime(DayOfWeek.valueOf, s.toUpperCase))

  implicit val duration: SExprDecoder2[Duration] =
    mapString(parseJavaTime(parsers.unsafeParseDuration, _))

  implicit val instant: SExprDecoder2[Instant] =
    mapString(parseJavaTime(parsers.unsafeParseInstant, _))

  implicit val localDate: SExprDecoder2[LocalDate] =
    mapString(parseJavaTime(parsers.unsafeParseLocalDate, _))

  implicit val localDateTime: SExprDecoder2[LocalDateTime] =
    mapString(parseJavaTime(parsers.unsafeParseLocalDateTime, _))

  implicit val localTime: SExprDecoder2[LocalTime] =
    mapString(parseJavaTime(parsers.unsafeParseLocalTime, _))

  implicit val month: SExprDecoder2[Month] =
    mapString(s => parseJavaTime(Month.valueOf, s.toUpperCase))

  implicit val monthDay: SExprDecoder2[MonthDay] =
    mapString(parseJavaTime(parsers.unsafeParseMonthDay, _))

  implicit val offsetDateTime: SExprDecoder2[OffsetDateTime] =
    mapString(parseJavaTime(parsers.unsafeParseOffsetDateTime, _))

  implicit val offsetTime: SExprDecoder2[OffsetTime] =
    mapString(parseJavaTime(parsers.unsafeParseOffsetTime, _))

  implicit val period: SExprDecoder2[Period] =
    mapString(parseJavaTime(parsers.unsafeParsePeriod, _))

  implicit val year: SExprDecoder2[Year] =
    mapString(parseJavaTime(parsers.unsafeParseYear, _))

  implicit val yearMonth: SExprDecoder2[YearMonth] =
    mapString(parseJavaTime(parsers.unsafeParseYearMonth, _))

  implicit val zonedDateTime: SExprDecoder2[ZonedDateTime] =
    mapString(parseJavaTime(parsers.unsafeParseZonedDateTime, _))

  implicit val zoneId: SExprDecoder2[ZoneId] =
    mapString(parseJavaTime(parsers.unsafeParseZoneId, _))

  implicit val zoneOffset: SExprDecoder2[ZoneOffset] =
    mapString(parseJavaTime(parsers.unsafeParseZoneOffset, _))

  // Commonized handling for decoding from string to java.time Class
  private[sexpr] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try Right(f(s))
    catch {
      case zre: ZoneRulesException      => Left(s"$s is not a valid ISO-8601 format, ${zre.getMessage}")
      case dtpe: DateTimeParseException => Left(s"$s is not a valid ISO-8601 format, ${dtpe.getMessage}")
      case dte: DateTimeException       => Left(s"$s is not a valid ISO-8601 format, ${dte.getMessage}")
      case ex: Exception                => Left(ex.getMessage)
    }

  implicit val uuid: SExprDecoder2[UUID] =
    mapString { str =>
      try Right(UUIDParser.unsafeParse(str))
      catch {
        case iae: IllegalArgumentException => Left(s"Invalid UUID: ${iae.getMessage}")
      }
    }
}
