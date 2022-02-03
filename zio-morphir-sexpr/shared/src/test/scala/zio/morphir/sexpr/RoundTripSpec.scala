package zio.morphir.sexpr

import zio.morphir.sexpr.Gens._
import zio.morphir.sexpr._
import zio.morphir.testing.ZioBaseSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

import java.time._

object RoundTripSpec extends ZioBaseSpec {
  def spec = suite("RoundTrip")(
    suite("primitives")(
      test("bigInt") {
        check(genBigInteger)(assertRoundtrips)
      } @@ samples(1000),
      test("bigDecimal") {
        check(genBigDecimal)(assertRoundtrips)
      } @@ samples(1000),
      test("boolean") {
        check(Gen.boolean)(assertRoundtrips)
      } @@ samples(1000),
      test("byte") {
        check(Gen.byte)(assertRoundtrips)
      } @@ samples(1000),
      test("char") {
        check(Gen.char)(assertRoundtrips)
      } @@ samples(1000),
      test("double") {
        check(Gen.double)(assertRoundtrips)
      } @@ samples(1000),
      test("float") {
        check(Gen.float)(assertRoundtrips)
      } @@ samples(1000),
      test("int") {
        check(Gen.int)(assertRoundtrips)
      } @@ samples(1000),
      test("long") {
        check(Gen.long)(assertRoundtrips)
      } @@ samples(1000),
      test("short") {
        check(Gen.short)(assertRoundtrips)
      } @@ samples(1000),
      test("string") {
        check(Gen.string)(assertRoundtrips)
      } @@ samples(1000)
    ),
    suite("java.time")(
      test("Duration") {
        check(Gen.finiteDuration)(assertRoundtrips)
      } @@ samples(1000),
      test("Instant") {
        check(Gen.instant)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalDate") {
        check(Gen.localDate)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalDateTime") {
        check(Gen.localDateTime)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalTime") {
        check(Gen.localTime)(assertRoundtrips)
      } @@ samples(1000),
      test("Month") {
        check(Gen.month)(assertRoundtrips)
      } @@ samples(1000),
      test("MonthDay") {
        check(Gen.monthDay)(assertRoundtrips)
      } @@ samples(1000),
      test("OffsetDateTime") {
        check(Gen.offsetDateTime)(assertRoundtrips)
      } @@ samples(1000),
      test("OffsetTime") {
        check(Gen.offsetTime)(assertRoundtrips)
      } @@ samples(1000),
      test("Period") {
        check(Gen.period)(assertRoundtrips)
      } @@ samples(1000),
      test("Year") {
        check(Gen.year)(assertRoundtrips)
      } @@ samples(1000),
      test("YearMonth") {
        check(Gen.yearMonth)(assertRoundtrips)
      } @@ samples(1000),
      test("ZoneId") {
        check(Gen.zoneId)(assertRoundtrips[ZoneId])
      },
      test("ZoneOffset") {
        check(Gen.zoneOffset)(assertRoundtrips[ZoneOffset])
      },
      test("ZonedDateTime") {
        check(Gen.zonedDateTime)(assertRoundtrips)
      } @@ samples(1000)
    ),
    test("UUID") {
      check(Gen.uuid)(assertRoundtrips)
    } @@ samples(1000),
    // test("Symbol") {
    //   check(symbolGen)(assertRoundtrips)
    // } @@ samples(1000),
    test("Option") {
      check(Gen.option(Gen.int))(assertRoundtrips)
    } @@ samples(1000)
  )

  // TODO: Be more complete, cover more cases
  val symbolGen = Gen
    .oneOf(
      Gen.alphaNumericString.filter(_.nonEmpty),
      Gen.alphaNumericString.map(str => "#" + str),
      Gen.const("."),
      Gen.const("/"),
      Gen.const("*")
    )
    .map(Symbol.apply)

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
