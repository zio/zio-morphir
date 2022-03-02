package zio.morphir.ir

import zio.Chunk
import zio.test.*
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.syntax.ValueSyntax

object ValueModuleSpec extends MorphirBaseSpec with ValueSyntax {
  def spec = suite("Value Module")(
    suite("Collect Variables should return as expected for:")(
      test("Apply") {
        val name  = Name.fromString("hello")
        val name2 = Name.fromString("world")
        val ff    = fieldFunction(name)
        val str   = string("string1")
        val str2  = string("string2")
        val rec   = record((name, str), (name2, str2))

        val expected = Set(name)
        assertTrue(
          apply(ff, rec).collectVariables == expected
        )
      }
//      test("Constructor") {},
//      test("Destructure") {},
//      test("Field") {},
//      test("FieldFunction") {},
//      test("IfThenElse") {},
//      test("Lambda") {},
//      test("LetDefinition") {},
//      test("LetRecursion") {},
//      test("List") {},
//      test("Literal") {},
//      test("NativeApply") {},
//      test("PatternMatch") {},
//      test("Reference") {},
//      test("Record") {},
//      test("Tuple") {},
//      test("Unit") {},
//      test("UpdateRecord") {},
//      test("Variable") {}
//    ),
//    suite("Collect Variables should return as expected for:")(
//      test("Apply") {},
//      test("Constructor") {},
//      test("Destructure") {},
//      test("Field") {},
//      test("FieldFunction") {},
//      test("IfThenElse") {},
//      test("Lambda") {},
//      test("LetDefinition") {},
//      test("LetRecursion") {},
//      test("List") {},
//      test("Literal") {},
//      test("NativeApply") {},
//      test("PatternMatch") {},
//      test("Reference") {},
//      test("Record") {},
//      test("Tuple") {},
//      test("Unit") {},
//      test("UpdateRecord") {},
//      test("Variable") {}
//    ),
//    suite("Collect Variables should return as expected for:")(
//      test("Apply") {},
//      test("Constructor") {},
//      test("Destructure") {},
//      test("Field") {},
//      test("FieldFunction") {},
//      test("IfThenElse") {},
//      test("Lambda") {},
//      test("LetDefinition") {},
//      test("LetRecursion") {},
//      test("List") {},
//      test("Literal") {},
//      test("NativeApply") {},
//      test("PatternMatch") {},
//      test("Reference") {},
//      test("Record") {},
//      test("Tuple") {},
//      test("Unit") {},
//      test("UpdateRecord") {},
//      test("Variable") {}
    )
  )
}
