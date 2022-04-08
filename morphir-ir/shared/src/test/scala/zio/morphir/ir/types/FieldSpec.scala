package zio.morphir.ir.types

import zio.morphir.ir._
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

object FieldSpec extends MorphirBaseSpec {
  def spec = suite("Field Spec")(
    suite("Field Construction")(
      test("A Field can be created from a string and TypeExpr using as") {
        val actual = "name" as TypeExpr.variable("String")
        assertTrue(
          actual == Field("name", TypeExpr.variable("String"))
        )
      },
      test("A Field can be created from a string and a Type using as") {
        val actual = "name" as Type.variable("String")
        assertTrue(
          actual == Field("name", Type.variable("String"))
        )
      }
    )
  )
}
