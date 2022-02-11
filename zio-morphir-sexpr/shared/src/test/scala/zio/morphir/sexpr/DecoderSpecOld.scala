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

object DecoderSpecOld extends ZioBaseSpec {
  def spec = suite("Decoder Old")(
    suite("fromSExprOld")(
      suite("primitives")(
        test("string") {
          assertTrue(
            "\"hello world\"".fromSExprOld[String] == Right("hello world"),
            "\"hello\\nworld\"".fromSExprOld[String] == Right("hello\nworld"),
            "\"hello\\rworld\"".fromSExprOld[String] == Right("hello\rworld"),
            "\"hello\\u0000world\"".fromSExprOld[String] == Right("hello\u0000world")
          )
          // "hello\u0000world".toSExpr == "\"hello\\u0000world\""
        },
        test("bigInt") {
          check(Gens.genBigInteger) { x =>
            assertTrue(x.toString.fromSExprOld[java.math.BigInteger] == Right(x))
          }
        },
        test("bigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assertTrue(x.toString.fromSExprOld[java.math.BigDecimal] == Right(x))
          }
        },
        test("boolean") {
          assertTrue("true".fromSExprOld[Boolean] == Right(true)) &&
          assertTrue("false".fromSExprOld[Boolean] == Right(false))
        }, // @@ ignore @@ tag("Something isn't working right!"),
        test("byte") {
          check(Gen.byte) { x =>
            assertTrue(x.toString.fromSExprOld[Byte] == Right(x))
          }
        },
        test("char") {
          check(Gen.char) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[Char] == Right(x))
          }
        },
        test("double") {
          check(Gen.double) { x =>
            assertTrue(x.toString.fromSExprOld[Double] == Right(x))
          }
        },
        test("float") {
          check(Gen.float) { x =>
            assertTrue(x.toString.fromSExprOld[Float] == Right(x))
          }
        },
        test("int") {
          check(Gen.int) { x =>
            assertTrue(x.toString.fromSExprOld[Int] == Right(x))
          }
        },
        test("long") {
          check(Gen.long) { x =>
            assertTrue(x.toString.fromSExprOld[Long] == Right(x))
          }
        },
        test("short") {
          check(Gen.short) { x =>
            assertTrue(x.toString.fromSExprOld[Short] == Right(x))
          }
        },
        suite("java.util.UUID")(
          test("Auto-generated") {
            check(Gen.uuid) { x =>
              assertTrue(s"\"${x.toString}\"".fromSExprOld[UUID] == Right(x))
            }
          },
          test("Manual") {
            val ok1  = """"64d7c38d-2afd-4514-9832-4e70afe4b0f8""""
            val ok2  = """"0000000064D7C38D-FD-14-32-70AFE4B0f8""""
            val ok3  = """"0-0-0-0-0""""
            val bad1 = """"""""
            val bad2 = """"64d7c38d-2afd-4514-9832-4e70afe4b0f80""""
            val bad3 = """"64d7c38d-2afd-4514-983-4e70afe4b0f80""""
            val bad4 = """"64d7c38d-2afd--9832-4e70afe4b0f8""""
            val bad5 = """"64d7c38d-2afd-XXXX-9832-4e70afe4b0f8""""
            val bad6 = """"64d7c38d-2afd-X-9832-4e70afe4b0f8""""
            val bad7 = """"0-0-0-0-00000000000000000""""

            assert(ok1.fromSExprOld[UUID])(
              isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
            ) &&
            assert(ok2.fromSExprOld[UUID])(
              isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AfE4B0f8")))
            ) &&
            assert(ok3.fromSExprOld[UUID])(
              isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))
            ) &&
            assert(bad1.fromSExprOld[UUID])(isLeft(containsString("Invalid UUID: "))) &&
            assert(bad2.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: UUID string too large"))
            ) &&
            assert(bad3.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
            ) &&
            assert(bad4.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
            ) &&
            assert(bad5.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
            ) &&
            assert(bad6.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
            ) &&
            assert(bad7.fromSExprOld[UUID])(
              isLeft(containsString("Invalid UUID: 0-0-0-0-00000000000000000"))
            )
          }
        )
      ),
      suite("java.time")(
        suite("Duration")(
          test("Auto-generated") {
            check(Gen.finiteDuration) { x =>
              assertTrue(s"\"${x.toString}\"".fromSExprOld[Duration] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = """"PT1H2M3S""""
            val ok2 =
              """"PT-0.5S"""" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
            val bad1 = """"PT-H""""

            assert(ok1.fromSExprOld[Duration])(
              isRight(equalTo(java.time.Duration.parse("PT1H2M3S")))
            ) &&
            assert(ok2.fromSExprOld[Duration])(
              isRight(equalTo(java.time.Duration.ofNanos(-500000000)))
            ) &&
            assert(bad1.fromSExprOld[Duration])(
              isLeft(
                containsString("PT-H is not a valid ISO-8601 format, expected digit at index 3")
              )
            )
          }
        ),
        test("Instant") {
          check(Gen.instant) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[Instant] == Right(x))
          }
        },
        test("LocalDate") {
          check(Gen.localDate) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[LocalDate] == Right(x))
          }
        },
        test("LocalDateTime") {
          check(Gen.localDateTime) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[LocalDateTime] == Right(x))
          }
        },
        test("LocalTime") {
          check(Gen.localTime) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[LocalTime] == Right(x))
          }
        },
        test("Month") {
          check(Gen.month) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[Month] == Right(x))
          }
        },
        test("MonthDay") {
          check(Gen.monthDay) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[MonthDay] == Right(x))
          }
        },
        test("OffsetDateTime") {
          check(Gen.offsetDateTime) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[OffsetDateTime] == Right(x))
          }
        },
        test("OffsetTime") {
          check(Gen.offsetTime) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[OffsetTime] == Right(x))
          }
        },
        test("Period") {
          check(Gen.period) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[Period] == Right(x))
          }
        },
        test("Year") {
          check(Gens.genYear) { x =>
            val year = "%04d".format(x.getValue)
            assertTrue(s"\"$year\"".fromSExprOld[Year] == Right(x))
          }
        },
        test("YearMonth") {
          check(Gens.genYearMonth) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[YearMonth] == Right(x))
          }
        },
        test("ZoneId") {
          check(Gen.zoneId) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[ZoneId] == Right(x))
          }
        },
        test("ZoneOffset") {
          check(Gen.zoneOffset) { x =>
            assertTrue(s"\"${x.toString}\"".fromSExprOld[ZoneOffset] == Right(x))
          }
        },
        suite("ZonedDateTime")(
          test("Auto-generated") {
            check(Gen.zonedDateTime) { x =>
              assertTrue(s"\"${x.toString}\"".fromSExprOld[ZonedDateTime] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = """"2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]""""
            val ok2 =
              """"2018-10-28T02:30+00:00[Europe/Warsaw]"""" // see https://bugs.openjdk.java.net/browse/JDK-8066982
            val bad1 = """"2018-10-28T02:30""""

            assert(ok1.fromSExprOld[ZonedDateTime])(
              isRight(
                equalTo(ZonedDateTime.parse("2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"))
              )
            ) &&
            assert(ok2.fromSExprOld[ZonedDateTime].map(_.toOffsetDateTime))(
              isRight(equalTo(OffsetDateTime.parse("2018-10-28T03:30+01:00")))
            ) &&
            assert(bad1.fromSExprOld[ZonedDateTime])(
              isLeft(
                equalTo(
                  "(2018-10-28T02:30 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
                )
              )
            )
          }
        )
      ),
      suite("Collections")(
        test("Seq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[Seq[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[Set[String]])(isRight(equalTo(expected)))
        },
        test("zio.Chunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(sexprStr.fromSExprOld[NonEmptyChunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk failure") {
          val sexprStr = "[]"

          assert(sexprStr.fromSExprOld[NonEmptyChunk[String]])(isLeft(equalTo("(Chunk was empty)")))
        },
        test("collections") {
          val arr = """[1, 2, 3]"""

          assert(arr.fromSExprOld[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
          assert(arr.fromSExprOld[IndexedSeq[Int]])(isRight(equalTo(IndexedSeq(1, 2, 3)))) &&
          assert(arr.fromSExprOld[immutable.LinearSeq[Int]])(
            isRight(equalTo(immutable.LinearSeq(1, 2, 3)))
          ) &&
          assert(arr.fromSExprOld[immutable.ListSet[Int]])(
            isRight(equalTo(immutable.ListSet(1, 2, 3)))
          ) &&
          assert(arr.fromSExprOld[immutable.TreeSet[Int]])(
            isRight(equalTo(immutable.TreeSet(1, 2, 3)))
          )
        }
      ),
      suite("fromAST")(
        test("BigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assert(SExpr.Num(x).asOld[java.math.BigDecimal])(isRight(equalTo(x)))
          }
        },
        //  TODO need encoders for these
        test("Seq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = Seq("5XL", "2XL", "XL")

          assert(sexpr.asOld[Seq[String]])(isRight(equalTo(expected)))
        },
        test("IndexedSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = IndexedSeq("5XL", "2XL", "XL")

          assert(sexpr.asOld[IndexedSeq[String]])(isRight(equalTo(expected)))
        },
        test("LinearSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = immutable.LinearSeq("5XL", "2XL", "XL")

          assert(sexpr.asOld[immutable.LinearSeq[String]])(isRight(equalTo(expected)))
        },
        test("ListSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = immutable.ListSet("5XL", "2XL", "XL")

          assert(sexpr.asOld[immutable.ListSet[String]])(isRight(equalTo(expected)))
        },
        test("TreeSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = immutable.TreeSet("5XL", "2XL", "XL")

          assert(sexpr.asOld[immutable.TreeSet[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = Vector("5XL", "2XL", "XL")

          assert(sexpr.asOld[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(sexpr.asOld[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(sexpr.asOld[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = Set("5XL", "2XL", "XL")
          assert(sexpr.asOld[Set[String]])(isRight(equalTo(expected)))
        },
        // test("Map") {
        //   val sExpr = SExpr.SMap(
        //     Map(
        //       SExpr.Str("5XL") -> SExpr.Num(java.math.BigDecimal(3)),
        //       SExpr.Str("2XL") -> SExpr.Num(java.math.BigDecimal(14)),
        //       SExpr.Str("XL")  -> SExpr.Num(java.math.BigDecimal(159))
        //     )
        //   )
        //   val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

        //   assert(sExpr.asOld[Map[String, Int]])(isRight(equalTo(expected)))
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
        //          assert(sExpr.asOld[SortedMap[String, Int]])(isRight(equalTo(expected)))
        //        },
        //        test("Map, custom keys") {
        //          val sExpr    = SExpr.SMap(Map(SExpr.Str("1") -> SExpr.Str("a"), SExpr.Str("2") -> SExpr.Str("b")))
        //          val expected = Map(1 -> "a", 2 -> "b")
        //
        //          assert(sExpr.asOld[Map[Int, String]])(isRight(equalTo(expected)))
        //        },
        test("zio.Chunk") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = Chunk("5XL", "2XL", "XL")

          assert(sexpr.asOld[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(sexpr.asOld[NonEmptyChunk[String]])(isRight(equalTo(expected)))
        },
        test("java.util.UUID") {
          val ok1  = SExpr.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f8")
          val ok2  = SExpr.Str("0000000064D7C38D-FD-14-32-70AFE4B0f8")
          val ok3  = SExpr.Str("0-0-0-0-0")
          val bad1 = SExpr.Str("")
          val bad2 = SExpr.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f80")
          val bad3 = SExpr.Str("64d7c38d-2afd-4514-983-4e70afe4b0f80")
          val bad4 = SExpr.Str("64d7c38d-2afd--9832-4e70afe4b0f8")
          val bad5 = SExpr.Str("64d7c38d-2afd-XXXX-9832-4e70afe4b0f8")
          val bad6 = SExpr.Str("64d7c38d-2afd-X-9832-4e70afe4b0f8")
          val bad7 = SExpr.Str("0-0-0-0-00000000000000000")

          assert(ok1.asOld[UUID])(
            isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
          ) &&
          assert(ok2.asOld[UUID])(
            isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AFE4B0f8")))
          ) &&
          assert(ok3.asOld[UUID])(
            isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))
          ) &&
          assert(bad1.asOld[UUID])(isLeft(containsString("Invalid UUID: "))) &&
          assert(bad2.asOld[UUID])(isLeft(containsString("Invalid UUID: UUID string too large"))) &&
          assert(bad3.asOld[UUID])(
            isLeft(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
          ) &&
          assert(bad4.asOld[UUID])(
            isLeft(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
          ) &&
          assert(bad5.asOld[UUID])(
            isLeft(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
          ) &&
          assert(bad6.asOld[UUID])(
            isLeft(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
          ) &&
          assert(bad7.asOld[UUID])(isLeft(containsString("Invalid UUID: 0-0-0-0-00000000000000000")))
        }
      ),
      test("Option") {
        assertTrue("nil".fromSExprOld[Option[Boolean]] == Right(None)) &&
        assertTrue("false".fromSExprOld[Option[Boolean]] == Right(Some(false))) &&
        assertTrue("true".fromSExprOld[Option[Boolean]] == Right(Some(true))) &&
        assertTrue("nil".fromSExprOld[Option[Int]] == Right(None)) &&
        assertTrue("26".fromSExprOld[Option[Int]] == Right(Some(26)))
      }
    )
  )
}
