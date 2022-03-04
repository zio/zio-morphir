package zio.morphir.ir

import zio.Chunk
import zio.test.*
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.syntax.ValueSyntax
import ValueModule.Value
import ValueModule.ValueCase.*
import zio.ZEnvironment

object ValueModuleSpec extends MorphirBaseSpec with ValueSyntax {
  def spec = suite("Value Module")(
    suite("Collect Variables should return as expected for:")(
      test("Apply") {
        val name  = Name.fromString("hello")
        val name2 = Name.fromString("world")
        val name3 = Name.fromString("planet")
        val ff    = fieldFunction(name3)
        val str   = string("string1")
        val str2  = string("string2")
        val rec   = record((name, str), (name2, str2))

        assertTrue(
          apply(ff, rec).collectVariables == Set(name3)
        )
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val constr = constructor(fqName)
        assertTrue(constr.collectVariables == Set())
      },
      test("Destructure") {
        val des = destructure(
          tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
          tuple(string("red"), string("blue")),
          variable("x")
        )
        assertTrue(des.collectVariables == Set(Name("x")))
      },
      test("Field") {
        val name = Name.fromString("Name")
        val fi   = field(string("String"), name)

        val name2 = Name.fromString("Name2")
        val name3 = Name.fromString("Name3")
        val fi2   = field(fieldFunction(name2), name3)

        assertTrue(
          fi.collectVariables == Set() &&
            fi2.collectVariables == Set(name2)
        )
      },
      test("FieldFunction") {
        val name = Name.fromString("Name")
        val ff   = fieldFunction(name)
        assertTrue(ff.collectVariables == Set(name))
      },
      test("IfThenElse") {
        val ife = ifThenElse(
          condition = literal(false),
          thenBranch = variable("y"),
          elseBranch = literal(3)
        )
        assertTrue(ife.collectVariables == Set(Name.fromString("y")))
      },
      test("Lambda") {
        val v1 = variable("x")

        val lam1 = lambda(
          asPattern(wildcardPattern, Name("x")),
          nativeApply(
            NativeFunction.Addition,
            Chunk(v1)
          )
        )
        val lam2 = lambda(
          asPattern(wildcardPattern, Name("x")),
          v1
        )
        assertTrue(
          lam1.collectVariables == Set() &&
            lam2.collectVariables == Set(Name("x"))
        )
      }
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
