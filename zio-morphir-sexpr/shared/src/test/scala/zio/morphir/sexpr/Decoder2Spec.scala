package zio.morphir.sexpr

import zio._
import zio.morphir.sexpr.ast._
import zio.morphir.testing.ZioBaseSpec
import zio.test.Assertion._
import zio.test._

import java.time._
import java.util.UUID
import java.time.LocalTime
import scala.collection.{SortedMap, immutable, mutable}
import scala.collection.immutable.*

object Decoder2Spec extends ZioBaseSpec {
  def wrap(s: String) = s""""$s""""

  def spec = suite("Decoder2")(
    suite("fromSExpr2")(
      suite("primitives")(
        test("string") {
          assertTrue(
            """"hello world"""".fromSExpr2[String] == Right("hello world")
            // """"hello\\nworld"""".fromSExpr2[String] == Right("hello\nworld"),
            // """"hello\\rworld"""""".fromSExpr2[String] == Right("hello\rworld"),
            // """"hello\\u0000world"""".fromSExpr2[String] == Right("hello\u0000world")
          )
        },
        test("bigInt") {
          check(Gens.genBigInteger) { x =>
            assertTrue(x.toString.fromSExpr2[java.math.BigInteger] == Right(x))
          }
        },
        test("bigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assertTrue(x.toString.fromSExpr2[java.math.BigDecimal] == Right(x))
          }
        },
        test("boolean") {
          assertTrue("true".fromSExpr2[Boolean] == Right(true)) &&
          assertTrue("false".fromSExpr2[Boolean] == Right(false))
        },
        test("byte") {
          check(Gen.byte) { x =>
            assertTrue(x.toString.fromSExpr2[Byte] == Right(x))
          }
        },
        test("char") {
          check(Gen.char) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[Char] == Right(x))
          }
        },
        test("double") {
          check(Gen.double) { x =>
            assertTrue(x.toString.fromSExpr2[Double] == Right(x))
          }
        },
        test("float") {
          check(Gen.float) { x =>
            assertTrue(x.toString.fromSExpr2[Float] == Right(x))
          }
        },
        test("int") {
          check(Gen.int) { x =>
            assertTrue(x.toString.fromSExpr2[Int] == Right(x))
          }
        },
        test("long") {
          check(Gen.long) { x =>
            assertTrue(x.toString.fromSExpr2[Long] == Right(x))
          }
        },
        test("short") {
          check(Gen.short) { x =>
            assertTrue(x.toString.fromSExpr2[Short] == Right(x))
          }
        }
      ),
      suite("java.util.UUID")(
        test("Auto-generated") {
          check(Gen.uuid) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[UUID] == Right(x))
          }
        },
        test("Manual") {
          val ok1             = "64d7c38d-2afd-4514-9832-4e70afe4b0f8"
          val ok2             = "0000000064D7C38D-FD-14-32-70AFE4B0f8"
          val ok3             = "0-0-0-0-0"
          val bad1            = ""
          val bad2            = "64d7c38d-2afd-4514-9832-4e70afe4b0f80"
          val bad3            = "64d7c38d-2afd-4514-983-4e70afe4b0f80"
          val bad4            = "64d7c38d-2afd--9832-4e70afe4b0f8"
          val bad5            = "64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"
          val bad6            = "64d7c38d-2afd-X-9832-4e70afe4b0f8"
          val bad7            = "0-0-0-0-00000000000000000"

          assertTrue(
            wrap(ok1).fromSExpr2[UUID] == Right(UUID.fromString(ok1)),
            wrap(ok2).fromSExpr2[UUID] == Right(UUID.fromString(ok2)),
            wrap(ok3).fromSExpr2[UUID] == Right(UUID.fromString(ok3)),
            wrap(bad1).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad1)"),
            wrap(bad2).fromSExpr2[UUID] == Left(s"(Invalid UUID: UUID string too large)"),
            wrap(bad3).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad3)"),
            wrap(bad4).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad4)"),
            wrap(bad5).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad5)"),
            wrap(bad6).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad6)"),
            wrap(bad7).fromSExpr2[UUID] == Left(s"(Invalid UUID: $bad7)")
          )
        }
      ),
      suite("java.time")(
        suite("Duration")(
          test("Auto-generated") {
            check(Gen.finiteDuration) { x =>
              assertTrue(s""""${x.toString}"""".fromSExpr2[Duration] == Right(x))
            }
          },
          test("Manual") {
            val ok1  = "PT1H2M3S"
            val ok2  = "PT-0.5S" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
            val bad1 = "PT-H"

            assertTrue(
              wrap(ok1).fromSExpr2[Duration] == Right(java.time.Duration.parse(ok1)),
              wrap(ok2).fromSExpr2[Duration] == Right(java.time.Duration.ofNanos(-500000000)),
              wrap(bad1).fromSExpr2[Duration] == Left(s"($bad1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          }
        ),
        test("DayOfWeek") {
          check(Gen.dayOfWeek) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[DayOfWeek] == Right(x))
          }
        },
        test("Instant") {
          check(Gen.instant) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[Instant] == Right(x))
          }
        },
        test("LocalDate") {
          check(Gen.localDate) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[LocalDate] == Right(x))
          }
        },
        test("LocalDateTime") {
          check(Gen.localDateTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[LocalDateTime] == Right(x))
          }
        },
        test("LocalTime") {
          check(Gen.localTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[LocalTime] == Right(x))
          }
        },
        test("Month") {
          check(Gen.month) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[Month] == Right(x))
          }
        },
        test("MonthDay") {
          check(Gen.monthDay) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[MonthDay] == Right(x))
          }
        },
        test("OffsetDateTime") {
          check(Gen.offsetDateTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[OffsetDateTime] == Right(x))
          }
        },
        test("OffsetTime") {
          check(Gen.offsetTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[OffsetTime] == Right(x))
          }
        },
        test("Period") {
          check(Gen.period) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[Period] == Right(x))
          }
        },
        test("Year") {
          check(Gens.genYear) { x =>
            val year = "%04d".format(x.getValue)
            assertTrue(s""""$year"""".fromSExpr2[Year] == Right(x))
          }
        },
        test("YearMonth") {
          check(Gens.genYearMonth) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[YearMonth] == Right(x))
          }
        },
        test("ZoneId") {
          check(Gen.zoneId) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[ZoneId] == Right(x))
          }
        },
        test("ZoneOffset") {
          check(Gen.zoneOffset) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr2[ZoneOffset] == Right(x))
          }
        },
        suite("ZonedDateTime")(
          test("Auto-generated") {
            check(Gen.zonedDateTime) { x =>
              assertTrue(s""""${x.toString}"""".fromSExpr2[ZonedDateTime] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = "2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"
            val ok2 = "2018-10-28T02:30+00:00[Europe/Warsaw]" // see https://bugs.openjdk.java.net/browse/JDK-8066982
            val result2 = "2018-10-28T03:30+01:00[Europe/Warsaw]"
            val bad1    = "2018-10-28T02:30"

            assertTrue(
              wrap(ok1).fromSExpr2[ZonedDateTime] == Right(ZonedDateTime.parse(ok1)),
              wrap(ok2).fromSExpr2[ZonedDateTime] == Right(ZonedDateTime.parse(result2)),
              wrap(bad1).fromSExpr2[ZonedDateTime] == Left(
                s"($bad1 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          }
        )
      ),
      suite("Collections")(
        test("Array") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Array("5XL", "2XL", "XL")

          assert(sexprStr.fromSExpr2[Array[String]])(isRight(equalTo(expected)))
        },
        test("List") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = List("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[List[String]] == Right(expected))
        },
        test("Seq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[Seq[String]] == Right(expected))
        },
        test("IndexedSeq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = IndexedSeq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[IndexedSeq[String]] == Right(expected))
        },
        test("LinearSeq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = LinearSeq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[LinearSeq[String]] == Right(expected))
        },
        test("Vector") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[Vector[String]] == Right(expected))
        },
        test("Set") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[Set[String]] == Right(expected))
        },
        test("HashSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[immutable.HashSet[String]] == Right(expected))
        },
        test("ListSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = ListSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[ListSet[String]] == Right(expected))
        },
        test("SortedSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = SortedSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[SortedSet[String]] == Right(expected))
        },
        test("TreeSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = TreeSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[TreeSet[String]] == Right(expected))
        },
        test("zio.Chunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[Chunk[String]] == Right(expected))
        },
        test("zio.NonEmptyChunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[NonEmptyChunk[String]] == Right(expected))
        },
        test("zio.NonEmptyChunk failure") {
          assertTrue("[]".fromSExpr2[NonEmptyChunk[String]] == Left("(Chunk was empty)"))
        },
        test("Iterable") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Iterable("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr2[immutable.Iterable[String]] == Right(expected))
        },
        test("Collections of Int") {
          val arr = """[1, 2, 3]"""
          assert(arr.fromSExpr2[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
          assertTrue(
            arr.fromSExpr2[List[Int]] == Right(List(1, 2, 3)),
            arr.fromSExpr2[Seq[Int]] == Right(Seq(1, 2, 3)),
            arr.fromSExpr2[IndexedSeq[Int]] == Right(IndexedSeq(1, 2, 3)),
            arr.fromSExpr2[LinearSeq[Int]] == Right(LinearSeq(1, 2, 3)),
            arr.fromSExpr2[Vector[Int]] == Right(Vector(1, 2, 3)),
            arr.fromSExpr2[Set[Int]] == Right(Set(1, 2, 3)),
            arr.fromSExpr2[HashSet[Int]] == Right(HashSet(1, 2, 3)),
            arr.fromSExpr2[ListSet[Int]] == Right(ListSet(1, 2, 3)),
            arr.fromSExpr2[SortedSet[Int]] == Right(SortedSet(1, 2, 3)),
            arr.fromSExpr2[TreeSet[Int]] == Right(TreeSet(1, 2, 3)),
            arr.fromSExpr2[Iterable[Int]] == Right(Iterable(1, 2, 3)),
            arr.fromSExpr2[zio.Chunk[Int]] == Right(zio.Chunk(1, 2, 3))
          )
        }
      ),
      test("Option") {
        assertTrue("nil".fromSExpr2[Option[Boolean]] == Right(None)) &&
        assertTrue("false".fromSExpr2[Option[Boolean]] == Right(Some(false))) &&
        assertTrue("true".fromSExpr2[Option[Boolean]] == Right(Some(true))) &&
        assertTrue("nil".fromSExpr2[Option[Int]] == Right(None)) &&
        assertTrue("26".fromSExpr2[Option[Int]] == Right(Some(26)))
      },
      suite("SExpr")(
        test("Bool") {
          assertTrue(
            "true".fromSExpr2[SExpr.Bool] == Right(SExpr.Bool.True),
            "false".fromSExpr2[SExpr.Bool] == Right(SExpr.Bool.False)
          )
        },
        test("Nil") {
          assertTrue(
            "nil".fromSExpr2[SExpr.Nil.type] == Right(SExpr.Nil)
          )
        },
        test("Str") {
          assertTrue(
            """"nil"""".fromSExpr2[SExpr.Str] == Right(SExpr.Str("nil")),
            """"null"""".fromSExpr2[SExpr.Str] == Right(SExpr.Str("null"))
            // """"\t\n\"""".fromSExpr2[SExpr.Str] == Right(SExpr.Str("\t\n"))
          )
        },
        test("Num") {
          assertTrue(
            "22.22".fromSExpr2[SExpr.Num] == Right(SExpr.Num(BigDecimal("22.22"))),
            "2222".fromSExpr2[SExpr.Num] == Right(SExpr.Num(BigDecimal("2222"))),
            "-2222".fromSExpr2[SExpr.Num] == Right(SExpr.Num(BigDecimal("-2222"))),
            "-22.22".fromSExpr2[SExpr.Num] == Right(SExpr.Num(BigDecimal("-22.22")))
          )
        },
        test("Symbol") {
          assertTrue(
            "symb1".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol("symb1", SymbolKind.Standard)),
            "null".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol("null", SymbolKind.Standard)),
            "#symb".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol("#symb", SymbolKind.Standard)),
            "_ns/il".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol("_ns/il", SymbolKind.Standard)),
            ".n/il".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol(".n/il", SymbolKind.Standard)),
            ":keyWord6".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol(":keyWord6", SymbolKind.Keyword)),
            "::keywrdMacro".fromSExpr2[SExpr.Symbol] == Right(SExpr.Symbol("::keywrdMacro", SymbolKind.Macro))
          )
        },
        test("SVector") {
          assertTrue(
            """[true -1 nil "Hello" null 3.14159 false]""".fromSExpr2[SExpr.SVector] == Right(
              SExpr.SVector(
                Chunk(
                  SExpr.Bool.True,
                  SExpr.Num(BigDecimal(-1)),
                  SExpr.Nil,
                  SExpr.Str("Hello"),
                  SExpr.Symbol("null"),
                  SExpr.Num(BigDecimal(3.14159)),
                  SExpr.Bool.False
                )
              )
            )
          )
        },
        test("SMap") {
          val vect = """{ "x" 0, :keyword1 "hello", ::#vect [1 2.1 -0.3333], nil true }"""
          assertTrue(
            vect.fromSExpr2[SExpr.SMap[SExpr, SExpr]] ==
              Right(
                SExpr.SMap(
                  Chunk(
                    SExpr.Str("x")                                -> SExpr.Num(BigDecimal(0)),
                    SExpr.Symbol(":keyword1", SymbolKind.Keyword) -> SExpr.Str("hello"),
                    SExpr.Symbol("::#vect", SymbolKind.Macro) -> SExpr.SVector(
                      Chunk(
                        SExpr.Num(BigDecimal(1)),
                        SExpr.Num(BigDecimal(2.1)),
                        SExpr.Num(BigDecimal(-0.3333))
                      )
                    ),
                    SExpr.Nil -> SExpr.Bool.True
                  ).toMap
                )
              )
          )
        }
      )
    ),
    suite("fromAST")(
      suite("primitives")(
        test("string") {
          assertTrue(
            """"hello world"""".fromSExpr2[String] == Right("hello world")
            // """"hello\\nworld"""".fromSExpr2[String] == Right("hello\nworld"),
            // """"hello\\rworld"""""".fromSExpr2[String] == Right("hello\rworld"),
            // """"hello\\u0000world"""".fromSExpr2[String] == Right("hello\u0000world")
          )
        },
        test("bigInt") {
          check(Gens.genBigInteger) { x =>
            assertTrue(SExpr.Num(x).as2[java.math.BigInteger] == Right(x))
          }
        },
        test("bigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assertTrue(SExpr.Num(x).as2[java.math.BigDecimal] == Right(x))
          }
        },
        test("boolean") {
          assertTrue(
            SExpr.Bool(true).as2[Boolean] == Right(true),
            SExpr.Bool(false).as2[Boolean] == Right(false)
          )
        },
        test("byte") {
          check(Gen.byte) { x =>
            assertTrue(SExpr.Num(x).as2[Byte] == Right(x))
          }
        },
        test("char") {
          check(Gen.char) { x =>
            assertTrue(SExpr.Str(x.toString).as2[Char] == Right(x))
          }
        },
        test("double") {
          check(Gen.double) { x =>
            assertTrue(SExpr.Num(x).as2[Double] == Right(x))
          }
        },
        test("float") {
          check(Gen.float) { x =>
            assertTrue(SExpr.Num(x).as2[Float] == Right(x))
          }
        },
        test("int") {
          check(Gen.int) { x =>
            assertTrue(SExpr.Num(x).as2[Int] == Right(x))
          }
        },
        test("long") {
          check(Gen.long) { x =>
            assertTrue(SExpr.Num(x).as2[Long] == Right(x))
          }
        },
        test("short") {
          check(Gen.short) { x =>
            assertTrue(SExpr.Num(x).as2[Short] == Right(x))
          }
        }
      ),
      suite("java.util.UUID")(
        test("Auto-generated") {
          check(Gen.uuid) { x =>
            assertTrue(SExpr.Str(x.toString).as2[UUID] == Right(x))
          }
        },
        test("Manual") {
          val ok1  = "64d7c38d-2afd-4514-9832-4e70afe4b0f8"
          val ok2  = "0000000064D7C38D-FD-14-32-70AFE4B0f8"
          val ok3  = "0-0-0-0-0"
          val bad1 = ""
          val bad2 = "64d7c38d-2afd-4514-9832-4e70afe4b0f80"
          val bad3 = "64d7c38d-2afd-4514-983-4e70afe4b0f80"
          val bad4 = "64d7c38d-2afd--9832-4e70afe4b0f8"
          val bad5 = "64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"
          val bad6 = "64d7c38d-2afd-X-9832-4e70afe4b0f8"
          val bad7 = "0-0-0-0-00000000000000000"

          assertTrue(
            SExpr.Str(ok1).as2[UUID] == Right(UUID.fromString(ok1)),
            SExpr.Str(ok2).as2[UUID] == Right(UUID.fromString(ok2)),
            SExpr.Str(ok3).as2[UUID] == Right(UUID.fromString(ok3)),
            SExpr.Str(bad1).as2[UUID] == Left(s"Invalid UUID: $bad1"),
            SExpr.Str(bad2).as2[UUID] == Left(s"Invalid UUID: UUID string too large"),
            SExpr.Str(bad3).as2[UUID] == Left(s"Invalid UUID: $bad3"),
            SExpr.Str(bad4).as2[UUID] == Left(s"Invalid UUID: $bad4"),
            SExpr.Str(bad5).as2[UUID] == Left(s"Invalid UUID: $bad5"),
            SExpr.Str(bad6).as2[UUID] == Left(s"Invalid UUID: $bad6"),
            SExpr.Str(bad7).as2[UUID] == Left(s"Invalid UUID: $bad7")
          )
        }
      ),
      suite("java.time")(
        suite("Duration")(
          test("Auto-generated") {
            check(Gen.finiteDuration) { x =>
              assertTrue(SExpr.Str(x.toString).as2[Duration] == Right(x))
            }
          },
          test("Manual") {
            val ok1  = "PT1H2M3S"
            val ok2  = "PT-0.5S" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
            val bad1 = "PT-H"

            assertTrue(
              SExpr.Str(ok1).as2[Duration] == Right(java.time.Duration.parse(ok1)),
              SExpr.Str(ok2).as2[Duration] == Right(java.time.Duration.ofNanos(-500000000)),
              SExpr.Str(bad1).as2[Duration] == Left("PT-H is not a valid ISO-8601 format, expected digit at index 3")
            )
          }
        ),
        test("DayOfWeek") {
          check(Gen.dayOfWeek) { x =>
            assertTrue(SExpr.Str(x.toString).as2[DayOfWeek] == Right(x))
          }
        },
        test("Instant") {
          check(Gen.instant) { x =>
            assertTrue(SExpr.Str(x.toString).as2[Instant] == Right(x))
          }
        },
        test("LocalDate") {
          check(Gen.localDate) { x =>
            assertTrue(SExpr.Str(x.toString).as2[LocalDate] == Right(x))
          }
        },
        test("LocalDateTime") {
          check(Gen.localDateTime) { x =>
            assertTrue(SExpr.Str(x.toString).as2[LocalDateTime] == Right(x))
          }
        },
        test("LocalTime") {
          check(Gen.localTime) { x =>
            assertTrue(SExpr.Str(x.toString).as2[LocalTime] == Right(x))
          }
        },
        test("Month") {
          check(Gen.month) { x =>
            assertTrue(SExpr.Str(x.toString).as2[Month] == Right(x))
          }
        },
        test("MonthDay") {
          check(Gen.monthDay) { x =>
            assertTrue(SExpr.Str(x.toString).as2[MonthDay] == Right(x))
          }
        },
        test("OffsetDateTime") {
          check(Gen.offsetDateTime) { x =>
            assertTrue(SExpr.Str(x.toString).as2[OffsetDateTime] == Right(x))
          }
        },
        test("OffsetTime") {
          check(Gen.offsetTime) { x =>
            assertTrue(SExpr.Str(x.toString).as2[OffsetTime] == Right(x))
          }
        },
        test("Period") {
          check(Gen.period) { x =>
            assertTrue(SExpr.Str(x.toString).as2[Period] == Right(x))
          }
        },
        test("Year") {
          check(Gens.genYear) { x =>
            assertTrue(SExpr.Str("%04d".format(x.getValue)).as2[Year] == Right(x))
          }
        },
        test("YearMonth") {
          check(Gens.genYearMonth) { x =>
            assertTrue(SExpr.Str(x.toString).as2[YearMonth] == Right(x))
          }
        },
        test("ZoneId") {
          check(Gen.zoneId) { x =>
            assertTrue(SExpr.Str(x.toString).as2[ZoneId] == Right(x))
          }
        },
        test("ZoneOffset") {
          check(Gen.zoneOffset) { x =>
            assertTrue(SExpr.Str(x.toString).as2[ZoneOffset] == Right(x))
          }
        },
        suite("ZonedDateTime")(
          test("Auto-generated") {
            check(Gen.zonedDateTime) { x =>
              assertTrue(SExpr.Str(x.toString).as2[ZonedDateTime] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = "2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"
            val ok2 = "2018-10-28T02:30+00:00[Europe/Warsaw]" // see https://bugs.openjdk.java.net/browse/JDK-8066982
            val result2 = "2018-10-28T03:30+01:00[Europe/Warsaw]"
            val bad1    = "2018-10-28T02:30"

            assertTrue(
              SExpr.Str(ok1.toString).as2[ZonedDateTime] == Right(ZonedDateTime.parse(ok1)),
              SExpr.Str(ok2.toString).as2[ZonedDateTime] == Right(ZonedDateTime.parse(result2)),
              SExpr.Str(bad1.toString).as2[ZonedDateTime] == Left(
                s"$bad1 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16"
              )
            )
          }
        )
      ),
      suite("Collections")(
        test("Collections of Int") {
          val sexpr = SExpr.vector(SExpr.Num(1), SExpr.Num(2), SExpr.Num(3), SExpr.Num(3))
          assertTrue(
            sexpr.as2[List[Int]] == Right(List(1, 2, 3, 3)),
            sexpr.as2[Seq[Int]] == Right(Seq(1, 2, 3, 3)),
            sexpr.as2[IndexedSeq[Int]] == Right(IndexedSeq(1, 2, 3, 3)),
            sexpr.as2[LinearSeq[Int]] == Right(LinearSeq(1, 2, 3, 3)),
            sexpr.as2[Vector[Int]] == Right(Vector(1, 2, 3, 3)),
            sexpr.as2[Set[Int]] == Right(Set(1, 2, 3)),
            sexpr.as2[HashSet[Int]] == Right(HashSet(1, 2, 3)),
            sexpr.as2[ListSet[Int]] == Right(ListSet(1, 2, 3)),
            sexpr.as2[SortedSet[Int]] == Right(SortedSet(1, 2, 3)),
            sexpr.as2[TreeSet[Int]] == Right(TreeSet(1, 2, 3)),
            sexpr.as2[Iterable[Int]] == Right(Iterable(1, 2, 3, 3)),
            sexpr.as2[zio.Chunk[Int]] == Right(zio.Chunk(1, 2, 3, 3))
          )
        },
        test("Array") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Array("5XL", "2XL", "XL", "2XL")

          assert(sexpr.as2[Array[String]])(isRight(equalTo(expected)))
        },
        test("List") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = List("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[List[String]] == Right(expected))
        },
        test("Seq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Seq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[Seq[String]] == Right(expected))
        },
        test("IndexedSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = IndexedSeq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[IndexedSeq[String]] == Right(expected))
        },
        test("LinearSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = LinearSeq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[LinearSeq[String]] == Right(expected))
        },
        test("Vector") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Vector("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[Vector[String]] == Right(expected))
        },
        test("Set") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Set("5XL", "2XL", "XL")
          assertTrue(sexpr.as2[Set[String]] == Right(expected))
        },
        test("HashSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as2[immutable.HashSet[String]] == Right(expected))
        },
        test("ListSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.ListSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as2[immutable.ListSet[String]] == Right(expected))
        },
        test("SortedSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as2[immutable.SortedSet[String]] == Right(expected))
        },
        test("TreeSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.TreeSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as2[immutable.TreeSet[String]] == Right(expected))
        },
        test("Iterable") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Iterable("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[Iterable[String]] == Right(expected))
        },
        test("zio.Chunk") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = zio.Chunk("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as2[zio.Chunk[String]] == Right(expected))
        }
      )
    )
    // test("Map") {
    //   val sExpr = SExpr.SMap(
    //     Map(
    //       SExpr.Str("5XL") -> SExpr.Num(java.math.BigDecimal(3)),
    //       SExpr.Str("2XL") -> SExpr.Num(java.math.BigDecimal(14)),
    //       SExpr.Str("XL")  -> SExpr.Num(java.math.BigDecimal(159))
    //     )
    //   )
    //   val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

    //   assertTrue(sExpr.as2[Map[String, Int]]== Right(expected))
    //   //          assertTrue(1 == 1)
    // },
    //        test("SortedMap") {
    //          val sExpr = SExpr.SMap(
    //            Map(
    //              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
    //              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
    //              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
    //            )
    //          )
    //          val expected = SortedMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)
    //
    //          assertTrue(sExpr.as2[SortedMap[String, Int]]== Right(expected))
    //        },
    //        test("Map, custom keys") {
    //          val sExpr    = SExpr.SMap(Map(SExpr.Str("1") -> SExpr.Str("a"), SExpr.Str("2") -> SExpr.Str("b")))
    //          val expected = Map(1 -> "a", 2 -> "b")
    //
    //          assertTrue(sExpr.as2[Map[Int, String]]== Right(expected))
    //        },
    //   test("zio.Chunk") {
    //     val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
    //     val expected = Chunk("5XL", "2XL", "XL")

    //     assertTrue(sexpr.as2[Chunk[String]]== Right(expected))
    //   },
    //   test("zio.NonEmptyChunk") {
    //     val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
    //     val expected = NonEmptyChunk("5XL", "2XL", "XL")

    //     assertTrue(sexpr.as2[NonEmptyChunk[String]]== Right(expected))
    //   },
    //   test("java.util.UUID") {
    //     val ok1  = SExpr.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f8")
    //     val ok2  = SExpr.Str("0000000064D7C38D-FD-14-32-70AFE4B0f8")
    //     val ok3  = SExpr.Str("0-0-0-0-0")
    //     val bad1 = SExpr.Str("")
    //     val bad2 = SExpr.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f80")
    //     val bad3 = SExpr.Str("64d7c38d-2afd-4514-983-4e70afe4b0f80")
    //     val bad4 = SExpr.Str("64d7c38d-2afd--9832-4e70afe4b0f8")
    //     val bad5 = SExpr.Str("64d7c38d-2afd-XXXX-9832-4e70afe4b0f8")
    //     val bad6 = SExpr.Str("64d7c38d-2afd-X-9832-4e70afe4b0f8")
    //     val bad7 = SExpr.Str("0-0-0-0-00000000000000000")

    //     assert(ok1.as2[UUID])(
    //       isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
    //     ) &&
    //     assert(ok2.as2[UUID])(
    //       isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AFE4B0f8")))
    //     ) &&
    //     assert(ok3.as2[UUID])(
    //       isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))
    //     ) &&
    //     assert(bad1.as2[UUID])(isLeft(containsString("Invalid UUID: "))) &&
    //     assert(bad2.as2[UUID])(isLeft(containsString("Invalid UUID: UUID string too large"))) &&
    //     assert(bad3.as2[UUID])(
    //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
    //     ) &&
    //     assert(bad4.as2[UUID])(
    //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
    //     ) &&
    //     assert(bad5.as2[UUID])(
    //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
    //     ) &&
    //     assert(bad6.as2[UUID])(
    //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
    //     ) &&
    //     assert(bad7.as2[UUID])(isLeft(containsString("Invalid UUID: 0-0-0-0-00000000000000000")))
    //   }
    // ),
  )
}
