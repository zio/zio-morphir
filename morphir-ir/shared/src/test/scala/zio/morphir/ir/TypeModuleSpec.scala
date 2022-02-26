package zio.morphir.ir

import testing.MorphirBaseSpec
import zio.morphir.ir.TypeModule.{Type, TypeCase}
import zio.test.*
import zio.morphir.syntax.TypeModuleSyntax

object TypeModuleSpec extends MorphirBaseSpec with TypeModuleSyntax {
  def spec = suite("Type")(
    suite("Reference")(),
    suite("Variable")(
      test("It should work as expected") {
        val actual = variable("FizzBuzz")
        assertTrue(actual == Type(TypeCase.VariableCase(Name.unsafeMake("fizz", "buzz"))))
      }
    )
  )
}
