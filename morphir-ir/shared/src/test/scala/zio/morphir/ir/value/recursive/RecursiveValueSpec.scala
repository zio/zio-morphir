package zio.morphir.ir.value.recursive

import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.ir.sdk.Basics.intType
import zio.morphir.ir.sdk.String.stringType
import zio.test._
import zio.morphir.ir.Name
import zio.morphir.ir.Type
object RecursiveValueSpec extends MorphirBaseSpec {
  import Value._
  import ValueCase._
  def spec: ZSpec[Environment, Any] = suite("Value Spec")(
    suite("Apply")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Constructor")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Destructure")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Field")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("FieldFunction")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a field name as a string") {
          val fieldName = "DayOfMonth"
          val actual    = fieldFunction(intType, fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction(intType, fieldName)
          )
        },
        test("It should be possible to construct given attributes and a field name") {
          val fieldName = Name.fromString("DayOfMonth")
          val actual    = fieldFunction(intType, fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction(intType, fieldName)
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a field name as a string") {
          val fieldName = "DayOfMonth"
          val actual    = fieldFunction(fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction.Raw(fieldName),
            actual == FieldFunction((), fieldName)
          )
        },
        test("It should be possible to construct given a field name") {
          val fieldName = Name.fromString("DayOfMonth")
          val actual    = fieldFunction(fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction.Raw(fieldName),
            actual == FieldFunction((), fieldName)
          )
        }
      )
    ),
    suite("IfThenElse")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Lambda")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("LetDefinition")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("LetRecursion")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("List")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Literal")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("PatternMatch")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Record")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Reference")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Tuple")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Unit")(
      suite("Attributed")(
        test("It should support construction given attributes") {
          val actual = unit(Type.unit)
          assertTrue(
            actual.attributes == Type.unit,
            actual == Unit(Type.unit),
            actual.toString() == "()"
          )
        }
      ),
      suite("Unattributed")(
        test("It should support construction given no attributes") {
          val actual = Value.unit
          assertTrue(
            actual.attributes == (),
            actual == Unit.Raw(),
            actual.toString() == "()"
          )
        }
      )
    ),
    suite("UpdateRecord")(
      suite("Attributed")(),
      suite("Unattributed")()
    ),
    suite("Variable")(
      suite("Attributed")(
        test("It should support construction given attributes and a name as a Sting") {
          val nameStr = "Alpha"
          val actual  = variable(stringType, nameStr)
          assertTrue(
            actual == Value(VariableCase(stringType, Name.fromString(nameStr))),
            actual.attributes == stringType,
            actual.toString == "alpha",
            actual == Variable(stringType, nameStr),
            actual match {
              case Variable(`stringType`, Name.VariableName("alpha")) => true
              case _                                                  => false
            }
          )
        },
        test("It should support construction given attributes and a name") {
          val name   = Name.fromString("Beta")
          val actual = variable(stringType, name)
          assertTrue(
            actual.attributes == stringType,
            actual.toString == "beta",
            actual == Value(VariableCase(stringType, name)),
            actual == Variable(stringType, name)
          )
        }
      ),
      suite("Unattributed")(
        test("It should support construction from a string value") {
          val nameStr = "Gamma"
          val actual  = variable(nameStr)
          assertTrue(
            actual == Value(VariableCase((), Name.fromString(nameStr))),
            actual.attributes == (),
            actual.toString == "gamma",
            actual == Variable.Raw(nameStr)
          )
        },
        test("It should support construction from a Name value") {
          val name   = Name.fromString("Epsilon")
          val actual = variable(name)
          assertTrue(
            actual == Value(VariableCase((), name)),
            actual.attributes == (),
            actual.toString == "epsilon",
            actual == Variable.Raw(name),
            actual.collectVariables == Set(name)
          )
        },
        test("foldLeft should work as expected on a variable value") {
          val actual = Variable.Raw(Name.fromString("foo"))
          assertTrue(
            actual.foldLeft(List.empty[RawValue])((acc, v) => v :: acc) == List(actual)
          )
        }
      )
    )
  )
}
