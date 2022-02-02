package zio.morphir.sexpr

import zio.morphir.sexpr.Gens._
import zio.morphir.sexpr._
import zio.morphir.testing.ZioBaseSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

import java.time._

object JavaTimeRoundtripSpec extends ZioBaseSpec {
  def spec = suite("JavaTimeRoundtripSpec")(
    suite("java.time")(
      test("Duration") {
        check(Gen.finiteDuration)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("Instant") {
        check(Gen.instant)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("LocalDate") {
        check(Gen.localDate)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("LocalDateTime") {
        check(Gen.localDateTime)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("LocalTime") {
        check(Gen.localTime)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("Month") {
        check(Gen.month)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("MonthDay") {
        check(Gen.monthDay)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("OffsetDateTime") {
        check(Gen.offsetDateTime)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("OffsetTime") {
        check(Gen.offsetTime)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("Period") {
        check(Gen.period)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("Year") {
        check(Gen.year)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("YearMonth") {
        check(Gen.yearMonth)(assertRoundtrips)
      } @@ jvm(samples(1000)),
      test("ZoneId") {
        check(Gen.zoneId)(assertRoundtrips[ZoneId])
      },
      test("ZoneOffset") {
        check(Gen.zoneOffset)(assertRoundtrips[ZoneOffset])
      },
      test("ZonedDateTime") {
        check(Gen.zonedDateTime)(assertRoundtrips)
      } @@ jvm(samples(1000))
    )
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
