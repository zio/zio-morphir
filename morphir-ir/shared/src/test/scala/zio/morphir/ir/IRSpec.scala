package zio.morphir.ir

import testing.MorphirBaseSpec

import zio.test.*

object IRSpec extends DefaultRunnableSpec {
  def spec = suite("IR")(typeSuite)

  val typeSuite = suite("Type")(
    suite("Reference")() +
      suite("Variable")(
        test("It should work as expected") {
          val actual = Type.variable("x")
          assertTrue(actual.name == Name("x"))
        }
      )
  )

}
