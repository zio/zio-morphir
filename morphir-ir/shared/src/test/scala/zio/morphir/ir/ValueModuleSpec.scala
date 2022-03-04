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
      },
      test("LetDefinition") {
        import ValueModule.ValueDefinition

        val ld = letDefinition(
          Name("y"),
          ValueDefinition.fromLiteral(int(2)),
          nativeApply(
            NativeFunction.Addition,
            Chunk(variable("x"), variable("y"))
          )
        )
        assertTrue(ld.collectVariables == Set(Name("y")))
      },
      test("LetRecursion") {
        val lr = letRecursion(
          Map(
            Name.fromString("x") -> ifThenElse(
              condition = literal(false),
              thenBranch = variable("y"),
              elseBranch = literal(3)
            ).toDefinition,
            Name.fromString("y") ->
              ifThenElse(
                condition = literal(false),
                thenBranch = literal(2),
                elseBranch = variable("z")
              ).toDefinition
          ),
          nativeApply(
            NativeFunction.Addition,
            Chunk(
              variable("a"),
              variable("b")
            )
          )
        )

        assertTrue(lr.collectVariables == Set(Name("x"), Name("y"), Name("z")))
      },
      test("List") {
        val list1 = list(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val list2 = list(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          list1.collectVariables == Set() &&
            list2.collectVariables == Set(Name("hello"))
        )
      },
      test("Literal") {
        val in = int(123)
        assertTrue(in.collectVariables == Set())
      },
      test("NativeApply") {
        val nat = nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
        assertTrue(nat.collectVariables == Set())
      },
      test("PatternMatch") {
        val cases = Chunk(
          (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
          (asPattern(wildcardPattern, Name.fromString("y")), variable(Name("integer")))
        )

        val pm = patternMatch(
          wholeNumber(new java.math.BigInteger("42")),
          cases
        )
        assertTrue(pm.collectVariables == Set(Name("name"), Name("integer")))
      },
      test("Reference") {
        val ref = reference(
          zio.morphir.ir.FQName(
            zio.morphir.ir.Path(Name("Morphir.SDK")),
            zio.morphir.ir.Path(Name("Morphir.SDK")),
            Name("RecordType")
          )
        )
        assertTrue(ref.collectVariables == Set())
      },
      test("Record") {
        val name  = Name.fromString("hello")
        val name2 = Name.fromString("world")
        val str   = string("string1")
        val va    = variable(name2)

        val rec = record(Chunk((name, str), (name2, va)))
        assertTrue(rec.collectVariables == Set(name2))
      },
      test("Tuple") {
        val tuple1 = tuple(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val tuple2 = tuple(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          tuple1.collectVariables == Set() &&
            tuple2.collectVariables == Set(Name("hello"))
        )
      },
      test("Unit") {
        assertTrue(unit.collectVariables == Set())
      },
      test("UpdateRecord") {
        val ur = updateRecord(
          string("hello world"),
          Chunk(
            Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
            Name("fieldC") -> variable(Name("none"))
          )
        )
        assertTrue(ur.collectVariables == Set(Name("none")))
      },
      test("Variable") {
        val name = Name("ha")
        assertTrue(variable(name).collectVariables == Set(name))
      }
    ),
    suite("Collect References should return as expected for:")(
      test("Apply") {
        val name  = FQName.fromString("hello:world", ":")
        val name2 = Name.fromString("wonderful")
        val ff    = reference(name)
        val str   = string("string1")
        val str2  = string("string2")
        val rec   = record((name2, str2))

        assertTrue(
          apply(ff, rec).collectReferences == Set(name)
        )
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val constr = constructor(fqName)
        assertTrue(constr.collectReferences == Set())
      },
      test("Destructure") {
        val fq = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val des = destructure(
          tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
          tuple(string("red"), reference(fq)),
          variable("x")
        )
        assertTrue(des.collectReferences == Set(fq))
      },
      test("Field") {
        val name = Name.fromString("Name")
        val fi   = field(string("String"), name)

        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val name2 = Name.fromString("Name3")
        val fi2   = field(reference(fqName), name2)

        assertTrue(
          fi.collectReferences == Set() &&
            fi2.collectReferences == Set(fqName)
        )
      }
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
