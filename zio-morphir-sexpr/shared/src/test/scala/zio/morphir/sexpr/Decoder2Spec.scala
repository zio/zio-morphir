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

object Decoder2Spec extends ZioBaseSpec {
  def spec = suite("Decoder2")(
    suite("fromSExpr")(
      suite("primitives")(
        test("string") {
          assertTrue(
            """"hello world"""".fromSExpr2[String] == Right("hello world")
            // """"hello\\nworld"""".fromSExpr2[String] == Right("hello\nworld"),
            // """"hello\\rworld\"""".fromSExpr2[String] == Right("hello\rworld"),
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
        //   suite("java.util.UUID")(
        //     test("Auto-generated") {
        //       check(Gen.uuid) { x =>
        //         assertTrue(s"\"${x.toString}\"".fromSExpr[UUID] == Right(x))
        //       }
        //     },
        //     test("Manual") {
        //       val ok1  = """"64d7c38d-2afd-4514-9832-4e70afe4b0f8""""
        //       val ok2  = """"0000000064D7C38D-FD-14-32-70AFE4B0f8""""
        //       val ok3  = """"0-0-0-0-0""""
        //       val bad1 = """"""""
        //       val bad2 = """"64d7c38d-2afd-4514-9832-4e70afe4b0f80""""
        //       val bad3 = """"64d7c38d-2afd-4514-983-4e70afe4b0f80""""
        //       val bad4 = """"64d7c38d-2afd--9832-4e70afe4b0f8""""
        //       val bad5 = """"64d7c38d-2afd-XXXX-9832-4e70afe4b0f8""""
        //       val bad6 = """"64d7c38d-2afd-X-9832-4e70afe4b0f8""""
        //       val bad7 = """"0-0-0-0-00000000000000000""""

        //       assert(ok1.fromSExpr[UUID])(
        //         isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
        //       ) &&
        //       assert(ok2.fromSExpr[UUID])(
        //         isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AfE4B0f8")))
        //       ) &&
        //       assert(ok3.fromSExpr[UUID])(
        //         isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))
        //       ) &&
        //       assert(bad1.fromSExpr[UUID])(isLeft(containsString("Invalid UUID: "))) &&
        //       assert(bad2.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: UUID string too large"))
        //       ) &&
        //       assert(bad3.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
        //       ) &&
        //       assert(bad4.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
        //       ) &&
        //       assert(bad5.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
        //       ) &&
        //       assert(bad6.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
        //       ) &&
        //       assert(bad7.fromSExpr[UUID])(
        //         isLeft(containsString("Invalid UUID: 0-0-0-0-00000000000000000"))
        //       )
        //     }
        //   )
        // ),
        // suite("java.time")(
        //   suite("Duration")(
        //     test("Auto-generated") {
        //       check(Gen.finiteDuration) { x =>
        //         assertTrue(s"\"${x.toString}\"".fromSExpr[Duration] == Right(x))
        //       }
        //     },
        //     test("Manual") {
        //       val ok1 = """"PT1H2M3S""""
        //       val ok2 =
        //         """"PT-0.5S"""" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
        //       val bad1 = """"PT-H""""

        //       assert(ok1.fromSExpr[Duration])(
        //         isRight(equalTo(java.time.Duration.parse("PT1H2M3S")))
        //       ) &&
        //       assert(ok2.fromSExpr[Duration])(
        //         isRight(equalTo(java.time.Duration.ofNanos(-500000000)))
        //       ) &&
        //       assert(bad1.fromSExpr[Duration])(
        //         isLeft(
        //           containsString("PT-H is not a valid ISO-8601 format, expected digit at index 3")
        //         )
        //       )
        //     }
        //   ),
        //   test("Instant") {
        //     check(Gen.instant) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[Instant] == Right(x))
        //     }
        //   },
        //   test("LocalDate") {
        //     check(Gen.localDate) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[LocalDate] == Right(x))
        //     }
        //   },
        //   test("LocalDateTime") {
        //     check(Gen.localDateTime) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[LocalDateTime] == Right(x))
        //     }
        //   },
        //   test("LocalTime") {
        //     check(Gen.localTime) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[LocalTime] == Right(x))
        //     }
        //   },
        //   test("Month") {
        //     check(Gen.month) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[Month] == Right(x))
        //     }
        //   },
        //   test("MonthDay") {
        //     check(Gen.monthDay) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[MonthDay] == Right(x))
        //     }
        //   },
        //   test("OffsetDateTime") {
        //     check(Gen.offsetDateTime) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[OffsetDateTime] == Right(x))
        //     }
        //   },
        //   test("OffsetTime") {
        //     check(Gen.offsetTime) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[OffsetTime] == Right(x))
        //     }
        //   },
        //   test("Period") {
        //     check(Gen.period) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[Period] == Right(x))
        //     }
        //   },
        //   test("Year") {
        //     check(Gens.genYear) { x =>
        //       val year = "%04d".format(x.getValue)
        //       assertTrue(s"\"$year\"".fromSExpr[Year] == Right(x))
        //     }
        //   },
        //   test("YearMonth") {
        //     check(Gens.genYearMonth) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[YearMonth] == Right(x))
        //     }
        //   },
        //   test("ZoneId") {
        //     check(Gen.zoneId) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[ZoneId] == Right(x))
        //     }
        //   },
        //   test("ZoneOffset") {
        //     check(Gen.zoneOffset) { x =>
        //       assertTrue(s"\"${x.toString}\"".fromSExpr[ZoneOffset] == Right(x))
        //     }
        //   },
        //   suite("ZonedDateTime")(
        //     test("Auto-generated") {
        //       check(Gen.zonedDateTime) { x =>
        //         assertTrue(s"\"${x.toString}\"".fromSExpr[ZonedDateTime] == Right(x))
        //       }
        //     },
        //     test("Manual") {
        //       val ok1 = """"2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]""""
        //       val ok2 =
        //         """"2018-10-28T02:30+00:00[Europe/Warsaw]"""" // see https://bugs.openjdk.java.net/browse/JDK-8066982
        //       val bad1 = """"2018-10-28T02:30""""

        //       assert(ok1.fromSExpr[ZonedDateTime])(
        //         isRight(
        //           equalTo(ZonedDateTime.parse("2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"))
        //         )
        //       ) &&
        //       assert(ok2.fromSExpr[ZonedDateTime].map(_.toOffsetDateTime))(
        //         isRight(equalTo(OffsetDateTime.parse("2018-10-28T03:30+01:00")))
        //       ) &&
        //       assert(bad1.fromSExpr[ZonedDateTime])(
        //         isLeft(
        //           equalTo(
        //             "(2018-10-28T02:30 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
        //           )
        //         )
        //       )
        //     }
        //   )
        // ),
        // suite("Collections")(
        //   test("Seq") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = Seq("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[Seq[String]])(isRight(equalTo(expected)))
        //   },
        //   test("Vector") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = Vector("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[Vector[String]])(isRight(equalTo(expected)))
        //   },
        //   test("SortedSet") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = immutable.SortedSet("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        //   },
        //   test("HashSet") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = immutable.HashSet("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[immutable.HashSet[String]])(isRight(equalTo(expected)))
        //   },
        //   test("Set") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = Set("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[Set[String]])(isRight(equalTo(expected)))
        //   },
        //   test("zio.Chunk") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = Chunk("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[Chunk[String]])(isRight(equalTo(expected)))
        //   },
        //   test("zio.NonEmptyChunk") {
        //     val sexprStr = """["5XL","2XL","XL"]"""
        //     val expected = NonEmptyChunk("5XL", "2XL", "XL")

        //     assert(sexprStr.fromSExpr[NonEmptyChunk[String]])(isRight(equalTo(expected)))
        //   },
        //   test("zio.NonEmptyChunk failure") {
        //     val sexprStr = "[]"

        //     assert(sexprStr.fromSExpr[NonEmptyChunk[String]])(isLeft(equalTo("(Chunk was empty)")))
        //   },
        //   test("collections") {
        //     val arr = """[1, 2, 3]"""

        //     assert(arr.fromSExpr[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
        //     assert(arr.fromSExpr[IndexedSeq[Int]])(isRight(equalTo(IndexedSeq(1, 2, 3)))) &&
        //     assert(arr.fromSExpr[immutable.LinearSeq[Int]])(
        //       isRight(equalTo(immutable.LinearSeq(1, 2, 3)))
        //     ) &&
        //     assert(arr.fromSExpr[immutable.ListSet[Int]])(
        //       isRight(equalTo(immutable.ListSet(1, 2, 3)))
        //     ) &&
        //     assert(arr.fromSExpr[immutable.TreeSet[Int]])(
        //       isRight(equalTo(immutable.TreeSet(1, 2, 3)))
        //     )
        //   }
      ),
      test("SMap") {
        assertTrue(
            """{ "x" 0, :keyword1 "hello", ::#vect [1 2.1 -0.3333], nil true }""".fromSExpr2[SExpr.SMap[SExpr,SExpr]]
           == Right(
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
      },
      suite("fromAST")(
        test("BigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assert(SExpr.Num(x).as2[java.math.BigDecimal])(isRight(equalTo(x)))
          }
        }
      ),
      //  TODO need encoders for these
      // test("Seq") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = Seq("5XL", "2XL", "XL")

      //   assert(sexpr.as[Seq[String]])(isRight(equalTo(expected)))
      // },
      // test("IndexedSeq") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = IndexedSeq("5XL", "2XL", "XL")

      //   assert(sexpr.as[IndexedSeq[String]])(isRight(equalTo(expected)))
      // },
      // test("LinearSeq") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = immutable.LinearSeq("5XL", "2XL", "XL")

      //   assert(sexpr.as[immutable.LinearSeq[String]])(isRight(equalTo(expected)))
      // },
      // test("ListSet") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = immutable.ListSet("5XL", "2XL", "XL")

      //   assert(sexpr.as[immutable.ListSet[String]])(isRight(equalTo(expected)))
      // },
      // test("TreeSet") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = immutable.TreeSet("5XL", "2XL", "XL")

      //   assert(sexpr.as[immutable.TreeSet[String]])(isRight(equalTo(expected)))
      // },
      // test("Vector") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = Vector("5XL", "2XL", "XL")

      //   assert(sexpr.as[Vector[String]])(isRight(equalTo(expected)))
      // },
      // test("SortedSet") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = immutable.SortedSet("5XL", "2XL", "XL")

      //   assert(sexpr.as[immutable.SortedSet[String]])(isRight(equalTo(expected)))
      // },
      // test("HashSet") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = immutable.HashSet("5XL", "2XL", "XL")

      //   assert(sexpr.as[immutable.HashSet[String]])(isRight(equalTo(expected)))
      // },
      // test("Set") {
      //   val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //   val expected = Set("5XL", "2XL", "XL")
      //   assert(sexpr.as[Set[String]])(isRight(equalTo(expected)))
      // },
      // test("Map") {
      //   val sExpr = SExpr.SMap(
      //     Map(
      //       SExpr.Str("5XL") -> SExpr.Num(java.math.BigDecimal(3)),
      //       SExpr.Str("2XL") -> SExpr.Num(java.math.BigDecimal(14)),
      //       SExpr.Str("XL")  -> SExpr.Num(java.math.BigDecimal(159))
      //     )
      //   )
      //   val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

      //   assert(sExpr.as[Map[String, Int]])(isRight(equalTo(expected)))
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
      //          assert(sExpr.as[SortedMap[String, Int]])(isRight(equalTo(expected)))
      //        },
      //        test("Map, custom keys") {
      //          val sExpr    = SExpr.SMap(Map(SExpr.Str("1") -> SExpr.Str("a"), SExpr.Str("2") -> SExpr.Str("b")))
      //          val expected = Map(1 -> "a", 2 -> "b")
      //
      //          assert(sExpr.as[Map[Int, String]])(isRight(equalTo(expected)))
      //        },
      //   test("zio.Chunk") {
      //     val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //     val expected = Chunk("5XL", "2XL", "XL")

      //     assert(sexpr.as[Chunk[String]])(isRight(equalTo(expected)))
      //   },
      //   test("zio.NonEmptyChunk") {
      //     val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
      //     val expected = NonEmptyChunk("5XL", "2XL", "XL")

      //     assert(sexpr.as[NonEmptyChunk[String]])(isRight(equalTo(expected)))
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

      //     assert(ok1.as[UUID])(
      //       isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
      //     ) &&
      //     assert(ok2.as[UUID])(
      //       isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AFE4B0f8")))
      //     ) &&
      //     assert(ok3.as[UUID])(
      //       isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))
      //     ) &&
      //     assert(bad1.as[UUID])(isLeft(containsString("Invalid UUID: "))) &&
      //     assert(bad2.as[UUID])(isLeft(containsString("Invalid UUID: UUID string too large"))) &&
      //     assert(bad3.as[UUID])(
      //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-4514-983-4e70afe4b0f80"))
      //     ) &&
      //     assert(bad4.as[UUID])(
      //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd--9832-4e70afe4b0f8"))
      //     ) &&
      //     assert(bad5.as[UUID])(
      //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"))
      //     ) &&
      //     assert(bad6.as[UUID])(
      //       isLeft(containsString("Invalid UUID: 64d7c38d-2afd-X-9832-4e70afe4b0f8"))
      //     ) &&
      //     assert(bad7.as[UUID])(isLeft(containsString("Invalid UUID: 0-0-0-0-00000000000000000")))
      //   }
      // ),
      test("Option") {
        assertTrue("nil".fromSExpr2[Option[Boolean]] == Right(None)) &&
        assertTrue("false".fromSExpr2[Option[Boolean]] == Right(Some(false))) &&
        assertTrue("true".fromSExpr2[Option[Boolean]] == Right(Some(true))) &&
        assertTrue("nil".fromSExpr2[Option[Int]] == Right(None)) &&
        assertTrue("26".fromSExpr2[Option[Int]] == Right(Some(26)))
      }
    )
  )
}
