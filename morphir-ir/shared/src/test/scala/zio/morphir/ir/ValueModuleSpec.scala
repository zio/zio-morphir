package zio.morphir.ir

import zio.Chunk
import zio.morphir.ir.Value.{Definition => ValueDefinition, Pattern, TypedValue}
import zio.morphir.ir.Value.Value.{Unit => UnitType, _}
import zio.morphir.ir.sdk.Basics.floatType
import zio.morphir.ir.{Literal => Lit}
import zio.morphir.ir.value.Pattern.LiteralPattern
import zio.morphir.testing.MorphirBaseSpec
// import zio.test.TestAspect.{ignore, tag}
import zio.test._
import zio.morphir.ir.Type.{Type => IrType, UType}

object ValueModuleSpec extends MorphirBaseSpec with value.ValueSyntax {

  val boolType: UType                  = IrType.ref(FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:Bool"))
  val intType: UType                   = sdk.Basics.intType
  def listType(itemType: UType): UType = IrType.reference(FQName.fromString("Morphir.SDK:List:List"), itemType)
  val stringType: UType                = sdk.String.stringType

  def spec = suite("Value Module")(
    suite("Collect Variables should return as expected for:")(
      test("Apply") {
        val ff  = fieldFunction("age")
        val rec = record("age" -> variable("myAge"), "firstName" -> string("John"))

        assertTrue(
          apply(ff, rec).collectVariables == Set(Name("myAge"))
        )
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val constr = constructor(fqName)
        assertTrue(constr.collectVariables == Set[Name]())
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

        assertTrue(
          field(variable("person"), "age").collectVariables == Set[Name](Name("person")),
          field(reference("Package:Module:People"), "count").collectVariables == Set.empty[Name]
        )
      },
      test("FieldFunction") {
        val ff = fieldFunction("name")
        assertTrue(ff.collectVariables == Set.empty[Name])
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
          list(v1, variable("y"))
        )
        val lam2 = lambda(
          asPattern(wildcardPattern, Name("x")),
          v1
        )
        assertTrue(
          lam1.collectVariables == Set[Name](Name("X"), Name("y")),
          lam2.collectVariables == Set(Name("x"))
        )
      },
      test("LetDefinition") {
        val yDef = definition()(intType)(int(42) :@ intType)

        val ld = letDefinition(
          "y",
          yDef,
          applyStrict(sdk.Basics.add(intType), variable("y", intType), int(42) :@ intType)
        )
        assertTrue(ld.collectVariables == Set(Name("y")))
      },
      test("LetRecursion") {
        val lr = LetRecursion.Typed(
          "x" -> IfThenElse
            .Typed(
              condition = Lit.False.toTypedValue,
              thenBranch = variable("y", intType),
              elseBranch = literal(3) :@ intType
            )
            .toDefinition,
          "y" ->
            IfThenElse
              .Typed(
                condition = literal(false) :@ boolType,
                thenBranch = literal(2) :@ intType,
                elseBranch = variable("z") :@ intType
              )
              .toDefinition
        )(
          apply(
            FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:add"),
            variable("x", intType),
            variable("y", intType)
          )(intType)
        )

        assertTrue(lr.collectVariables == Set(Name("x"), Name("y"), Name("z")))
      },
      test("List") {
        val list1 = listOf(stringType)(literal("hello"), literal("world"))
        val list2 = list(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          list1.collectVariables == Set[Name]() &&
            list2.collectVariables == Set(Name("hello"))
        )
      },
      test("Literal") {
        val in = int(123)
        assertTrue(in.collectVariables == Set[Name]())
      },
      test("NativeApply") {
        val nat = nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
        assertTrue(nat.collectVariables == Set(Name("x"), Name("y")))
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
        assertTrue(ref.collectVariables == Set[Name]())
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
        val tuple1 = Tuple.Typed(literal("hello"), literal("world"))
        val tuple2 = tuple(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          tuple1.collectVariables == Set[Name]() &&
            tuple2.collectVariables == Set(Name("hello"))
        )
      },
      test("Unit") {
        assertTrue(unit.collectVariables == Set[Name]())
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
        val str   = string("string2")
        val rec   = record((name2, str))

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
        assertTrue(constr.collectReferences == Set[FQName]())
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
          fi.collectReferences == Set[FQName]() &&
            fi2.collectReferences == Set(fqName)
        )
      },
      test("FieldFunction") {
        val name = Name.fromString("Name")
        val ff   = fieldFunction(name)
        assertTrue(ff.collectReferences == Set[FQName]())
      },
      test("IfThenElse") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val fqName2 = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("VariableType")
        )
        val ife = ifThenElse(
          condition = reference(fqName),
          thenBranch = variable("y"),
          elseBranch = reference(fqName2)
        )
        assertTrue(ife.collectReferences == Set(fqName, fqName2))
      },
      test("Lambda") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )

        val lam1 = lambda(
          asPattern(wildcardPattern, Name("x")),
          reference(fqName)
        )
        val lam2 = lambda(
          asPattern(wildcardPattern, Name("x")),
          variable("x")
        )
        assertTrue(
          lam1.collectReferences == Set(fqName) &&
            lam2.collectReferences == Set[FQName]()
        )
      },
      test("LetDefinition") {

        val fqName  = FQName.fromString("Morphir:SDK:valueType")
        val fqName2 = FQName.fromString("Morphir:SDK:typed")

        val ld = letDefinition(
          Name("y"),
          ValueDefinition()(intType)(reference(fqName)),
          tuple(
            int(42),
            reference(fqName2)
          )
        )
        assertTrue(ld.collectReferences == Set(fqName, fqName2))
      },
      test("LetRecursion") {
        val fqName = FQName.fromString("Zio:Morphir.Basics:constInt")
        val lr = LetRecursion.Typed(
          "x" -> IfThenElse
            .Typed(
              condition = Lit.False.toTypedValue,
              thenBranch = reference(fqName, intType),
              elseBranch = Lit.int(42).toTypedValue
            )
            .toDefinition,
          "y" ->
            IfThenElse
              .Typed(
                condition = Lit.False.toTypedValue,
                thenBranch = Lit.int(2).toTypedValue,
                elseBranch = Lit.int(42).toTypedValue
              )
              .toDefinition
        )(variable("y", intType))

        assertTrue(lr.collectReferences == Set(fqName))
      },
      test("List") {
        val list1 = listOf(stringType)(literal("hello"), literal("world"))
        val fq    = FQName.fromString("hello:world:star")
        val list2 = list(
          Chunk(
            reference(fq),
            int(3)
          )
        )
        assertTrue(
          list1.collectReferences == Set[FQName]() &&
            list2.collectReferences == Set(fq)
        )
      },
      test("Literal") {
        val in = int(123)
        assertTrue(in.collectReferences == Set[FQName]())
      },
      test("NativeApply") {
        val nat = nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
        assertTrue(nat.collectReferences == Set[FQName]())
      },
      test("PatternMatch") {
        val fq  = FQName.fromString("hello:world:star", ":")
        val fq2 = FQName.fromString("hello:world:mission", ":")
        val cases = Chunk(
          (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
          (asPattern(wildcardPattern, Name.fromString("y")), reference(fq2))
        )

        val pm = patternMatch(
          reference(fq),
          cases
        )
        assertTrue(pm.collectReferences == Set(fq, fq2))
      },
      test("Reference") {
        val fq = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val ref = reference(fq)
        assertTrue(ref.collectReferences == Set(fq))
      },
      test("Record") {
        val name   = Name.fromString("hello")
        val name2  = Name.fromString("world")
        val fqName = FQName.fromString("folder:location:name", ":")
        val str    = string("string1")
        val rf     = reference(fqName)

        val rec = record(Chunk((name, str), (name2, rf)))
        assertTrue(rec.collectReferences == Set(fqName))
      },
      test("Tuple") {
        val tuple1 = Tuple.Typed(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val fq = FQName.fromString("hello:world:star", ":")
        val tuple2 = tuple(
          Chunk(
            reference(fq),
            int(3)
          )
        )
        assertTrue(
          tuple1.collectReferences == Set[FQName]() &&
            tuple2.collectReferences == Set(fq)
        )
      },
      test("Unit") {
        assertTrue(unit.collectReferences == Set[FQName]())
      },
      test("UpdateRecord") {
        val fq = FQName.fromString("hello:world:string", ":")
        val ur = updateRecord(
          string("hello world"),
          Chunk(
            Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
            Name("fieldC") -> reference(fq)
          )
        )
        assertTrue(ur.collectReferences == Set(fq))
      },
      test("Variable") {
        assertTrue(variable(Name("name")).collectReferences == Set[FQName]())
      }
    ),
    suite("toRawValue should return as expected for:")(
      test("Apply") {
        val function = Reference.Typed("Test:Test:square")(floatType)
        val in       = Apply.Typed(function, Lit.float(2.0f).toTypedValue)

        assertTrue(in.toRawValue == Apply.Raw(function.toRawValue, Lit.float(2.0f).toRawValue))
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val typeRef = IrType.ref(fqName)
        val constr  = Constructor.Typed(fqName)(typeRef)
        assertTrue(constr.toRawValue == constructor(fqName))
      },
      test("Destructure") {
        val lit    = Lit.string("timeout")
        val lit2   = Lit.string("username")
        val value  = Value.Value.Literal(stringType, lit)
        val value2 = Value.Value.Literal(stringType, lit2)

        val des: TypedValue =
          Destructure(
            stringType,
            Pattern.WildcardPattern(stringType),
            value,
            value2
          )
        assertTrue(
          des.toRawValue == destructure(
            Pattern.WildcardPattern(stringType),
            string("timeout"),
            string("username")
          )
        )
      },
      test("Field") {
        val name  = Name.fromString("Name")
        val value = Value.Value.Literal.Typed(Lit.int(42))(intType)

        val actual = Field.Typed(intType, value, name)

        assertTrue(
          actual.toRawValue == field(int(42), name)
        )
      },
      test("FieldFunction") {
        val age = Name.fromString("age")
        val ff  = FieldFunction.Typed(age)(intType)

        assertTrue(ff.toRawValue == fieldFunction(age))
      },
      test("IfThenElse") {
        val gt: TypedValue        = reference(FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:greaterThan"), intType)
        val x: TypedValue         = variable("x", intType)
        val y: TypedValue         = variable("y", intType)
        val condition: TypedValue = applyStrict(gt, x, y)

        val ife = IfThenElse.Typed(condition, x, y)
        assertTrue(ife.toRawValue == IfThenElse.Raw(condition.toRawValue, x.toRawValue, y.toRawValue))
      },
      test("Lambda") {

        val actual = Lambda.Typed(
          Pattern.asPattern(intType, wildcardPattern(intType), Name.fromString("x")),
          variable(Name.fromString("x"), intType)
        )

        assertTrue(
          actual.toRawValue == lambda(
            pattern = Pattern.asPattern(intType, wildcardPattern, Name.fromString("x")),
            body = variable(Name.fromString("x"))
          )
        )
      },
      test("LetDefinition") {
        val value   = Lit.False.toTypedValue
        val flagDef = definition()(boolType)(value)

        val ld = LetDefinition.Typed("flag", flagDef, variable("flag", boolType))
        assertTrue(
          ld.toRawValue == LetDefinition.Raw(
            "flag",
            ValueDefinition.Raw()(boolType)(value.toRawValue),
            variable("flag").toRawValue
          )
        )
      },
      test("LetRecursion") {

        val times = Reference(1, FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:multiply"))
        val body  = Apply(4, times, Variable(5, "x"), Variable(6, "y"))

        val lr = LetRecursion(
          0,
          "x" -> ValueDefinition.fromLiteral(1, Lit.int(0)),
          "y" -> ValueDefinition.fromLiteral(2, Lit.int(42))
        )(body)

        assertTrue(
          lr.toRawValue == LetRecursion.Raw(
            "x" -> ValueDefinition.Raw()(intType)(Lit.int(0)),
            "y" -> ValueDefinition.Raw()(intType)(Lit.int(42))
          )(Apply.Raw(times.toRawValue, Variable.Raw("x"), Variable.Raw("y")))
        )
      },
      test("List") {

        val l1 = List.Typed(Lit.True.toTypedValue, Lit.False.toTypedValue)(listType(boolType))

        assertTrue(
          l1.toRawValue == list(boolean(true), boolean(false))
        )
      },
      test("Literal") {
        val value = Lit.True.toTypedValue

        assertTrue(value.toRawValue == boolean(true))
      },
      test("NativeApply") {
        val x      = Lit.int(42).toTypedValue
        val y      = Lit.int(58).toTypedValue
        val addRef = sdk.Basics.add(x.attributes)

        val actual = Apply.Typed(addRef, x, y)
        assertTrue(actual.toRawValue == apply(addRef.toRawValue, x.toRawValue, y.toRawValue))
      },
      test("PatternMatch") {
        val input = Variable.Typed("magicNumber")(intType)
        val yes   = string("yes") :@ stringType
        val no    = string("no") :@ stringType
        val n42   = Lit.int(42)

        val pm = caseOf(input)(
          LiteralPattern.Typed(n42)(intType) -> yes,
          wildcardPattern(yes.attributes)    -> no
        )
        assertTrue(
          pm.toRawValue == PatternMatch.Raw(
            input.toRawValue,
            LiteralPattern.Raw(n42) -> yes.toRawValue,
            Pattern.wildcardPattern -> no.toRawValue
          )
        )
      },
      test("Reference") {

        val intTypeName = FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:Int")
        val ref         = Reference.Typed(intTypeName)(zio.morphir.ir.sdk.Basics.intType)
        assertTrue(ref.toRawValue == Reference.Raw(intTypeName))
      },
      test("Record") {
        val name       = Name.fromString("hello")
        val lit        = string("timeout") :@ stringType
        val recordType = Type.record(Type.field("hello", stringType))
        val rec        = Record(recordType, Chunk(name -> lit))

        assertTrue(rec.toRawValue == record(Chunk((name, string("timeout")))))
      },
      test("Tuple") {

        val t1 = Tuple(string("shimmy") -> stringType)
        assertTrue(
          t1.toRawValue == Tuple.Raw(string("shimmy"))
        )
      },
      test("UpdateRecord") {

        val greeter = variable("greeter") :@ Type.record(Type.field("greeting", stringType))
        val actual  = UpdateRecord.Typed(greeter, ("greeting", string("world") :@ stringType))

        assertTrue(
          actual.toRawValue == UpdateRecord.Raw(Variable.Raw("greeter"), "greeting" -> string("world"))
        )
      },
      test("Variable") {
        val name  = Name("ha")
        val value = Variable(stringType, name)
        assertTrue(value.toRawValue == variable(name))
      },
      test("Unit") {
        assertTrue(UnitType(Type.unit).toRawValue == unit)
      }
    )
  )
}
