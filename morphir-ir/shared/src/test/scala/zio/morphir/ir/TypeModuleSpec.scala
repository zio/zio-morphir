package zio.morphir.ir

import testing.MorphirBaseSpec
import zio.morphir.ir.TypeModule.{Type, TypeCase}
import zio.test.*
import zio.morphir.syntax.TypeModuleSyntax

object TypeModuleSpec extends MorphirBaseSpec with TypeModuleSyntax {
  def spec = suite("Type")(
    suite("Operations")(
      test("Can be documented") {
        val actual = variable("a") ?? "Some type variable"
        assertTrue(actual.doc == "Some type variable")
      }
    ),
    suite("Reference")(),
    suite("Variable")(
      test("It should work as expected") {
        val actual = variable("FizzBuzz")
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz")))
      }
    )
  )
}
