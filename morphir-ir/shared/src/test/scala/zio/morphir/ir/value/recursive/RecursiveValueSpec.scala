package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.ir.{FQName, Gens, Name, Type}
import zio.morphir.ir.sdk.Basics.{boolType, floatType, intType}
//import zio.morphir.ir.sdk.Maybe.maybe
import zio.morphir.ir.sdk.String.stringType
import zio.test._

object RecursiveValueSpec extends MorphirBaseSpec {
  import Value._
  import ValueCase._
  def spec: ZSpec[Environment, Any] = suite("Value Spec")(
    suite("Apply")(
      suite("Attributed")(
        test("It should be possible to create a single argument function application") {
          val attribute   = "int -> string"
          val intToString = reference(stringType, "Morphir.SDK", "Int", "intToString")
          val actual      = apply(attribute, intToString, int(42))

          assertTrue(
            actual == Apply(attribute, intToString, int(42)),
            actual == Value(ApplyCase(attribute, intToString, int(42))),
            actual.attributes == attribute,
            actual.toString == "Morphir.SDK.Int.intToString 42"
          )
        },
        test("It should be possible to create a multi argument function application") {
          val attribute = "int -> int"
          val max       = reference(intType, "Morphir.SDK", "Basics", "max")
          val actual    = apply(attribute, apply("int -> int -> int", max, int(1)), int(2))

          assertTrue(
            actual == Apply(attribute, Apply("int -> int -> int", max, int(1)), int(2)),
            actual.attributes == attribute,
            actual.toString == "Morphir.SDK.Basics.max 1 2"
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create a single argument function application") {
          val intToString = reference("Morphir.SDK", "Int", "intToString")
          val actual      = apply(intToString, int(100))

          assertTrue(
            actual == Apply.Raw(intToString, int(100)),
            actual == Value(ApplyCase((), intToString, int(100))),
            actual.toString == "Morphir.SDK.Int.intToString 100"
          )
        },
        test("It should be possible to create a multi argument function application") {
          val max    = reference("Morphir.SDK", "Basics", "max")
          val actual = apply(apply(max, int(1)), int(2))

          assertTrue(
            actual == Apply.Raw(Apply.Raw(max, int(1)), int(2)),
            actual.toString == "Morphir.SDK.Basics.max 1 2"
          )
        }
      )
    ),
    suite("Constructor")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a FQ name as a string") {
          val fqName     = "Morphir.SDK:Maybe:Just"
          val attributes = "Maybe"
          val actual     = constructor(attributes, fqName)
          assertTrue(
            actual == Constructor(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.SDK.Maybe.Just"
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName     = FQName.fqn("Morphir.SDK", "Maybe", "Just")
          val attributes = "Maybe"
          val actual     = constructor(attributes, fqName)
          assertTrue(
            actual == Constructor(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.SDK.Maybe.Just"
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a FQ name as a string") {
          val fqName = "Morphir:Morphir.SDK.Maybe:Just"
          val actual = constructor(fqName)
          assertTrue(
            actual == Constructor.Raw(fqName),
            actual == Constructor((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.Just"
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "Nothing")
          val actual = constructor(fqName)
          assertTrue(
            actual == Constructor.Raw(fqName),
            actual == Constructor((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.Nothing"
          )
        }
      )
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
      suite("Attributed")(
        test("It should be possible to create an empty list with only attributes") {
          val actual = list(intType)
          assertTrue(
            actual == List(intType),
            actual.attributes == intType,
            actual.toString == "[]"
          )
        },
        test("It should be possible to create a list with only attributes and a single element") {
          val element = decimal(BigDecimal(3.99))
          val actual  = list(floatType, element)
          assertTrue(
            actual == List(floatType, element),
            actual == List(floatType, Chunk(element)),
            actual.attributes == floatType,
            actual.toString == "[3.99]"
          )
        },
        test("It should be possible to create a list with attributes and multiple elements") {
          val element1 = decimal(BigDecimal(3.99))
          val element2 = decimal(BigDecimal(4.99))
          val element3 = decimal(BigDecimal(5.99))
          val element4 = decimal(BigDecimal(6.99))
          val actual   = list(floatType, element1, element2, element3, element4)
          assertTrue(
            actual == List(floatType, element1, element2, element3, element4),
            actual == List(floatType, Chunk(element1, element2, element3, element4)),
            actual.attributes == floatType,
            actual.toString == "[3.99, 4.99, 5.99, 6.99]"
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create an empty list") {
          val actual = list()
          assertTrue(
            actual == List.Raw(),
            actual == List((), Chunk.empty),
            actual.attributes == (),
            actual.toString == "[]"
          )
        },
        test("It should be possible to create a list with a single element") {
          val element = decimal(BigDecimal(3.99))
          val actual  = list(element)
          assertTrue(
            actual == List.Raw(element),
            actual == List((), Chunk(element)),
            actual.attributes == (),
            actual.toString == "[3.99]"
          )
        },
        test("It should be possible to create a list with multiple elements") {
          val element1 = decimal(BigDecimal(3.99))
          val element2 = decimal(BigDecimal(4.99))
          val element3 = decimal(BigDecimal(5.99))
          val element4 = decimal(BigDecimal(6.99))
          val actual   = list(element1, element2, element3, element4)
          assertTrue(
            actual == List.Raw(element1, element2, element3, element4),
            actual == List((), Chunk(element1, element2, element3, element4)),
            actual.attributes == (),
            actual.toString == "[3.99, 4.99, 5.99, 6.99]"
          )
        }
      )
    ),
    suite("Literal")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a literal value") {
          check(Gens.literal) { givenLiteral =>
            val inferredType = givenLiteral.inferredType
            val actual       = literal(inferredType, givenLiteral)
            assertTrue(
              actual.toString == givenLiteral.toString(),
              actual.attributes == inferredType,
              actual == Literal(inferredType, givenLiteral)
            )
          }
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a literal value") {
          check(Gens.literal) { givenLiteral =>
            val actual = literal(givenLiteral)
            assertTrue(
              actual.toString == givenLiteral.toString(),
              actual.attributes == (),
              actual == Literal.Raw(givenLiteral),
              actual == Literal((), givenLiteral)
            )
          }
        }
      )
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
      suite("Attributed")(
        test("It should be possible to construct given attributes and a FQ name as a string") {
          val fqName     = "Morphir:Morphir.SDK.Maybe:just"
          val attributes = "Maybe"
          val actual     = reference(attributes, fqName)
          assertTrue(
            actual == Reference(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just"
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName     = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "just")
          val attributes = "Maybe"
          val actual     = reference(attributes, fqName)
          assertTrue(
            actual == Reference(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just"
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a FQ name as a string") {

          val fqName = "Morphir:Morphir.SDK.Maybe:Just"
          val actual = reference(fqName)
          assertTrue(
            actual == Reference.Raw(fqName),
            actual == Reference((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just"
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "Nothing")
          val actual = reference(fqName)
          assertTrue(
            actual == Reference.Raw(fqName),
            actual == Reference((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.nothing"
          )
        }
      )
    ),
    suite("Tuple")(
      suite("Attributed")(
        test("It should be possible to construct an empty tuple only given attributes") {
          val attributes = "EmptyTuple"
          val actual     = emptyTuple(attributes)
          assertTrue(
            actual == Tuple(attributes, Chunk.empty),
            actual == tuple(attributes, Chunk.empty),
            actual.attributes == "EmptyTuple",
            actual.toString() == "()"
          )
        },
        test("It should be possible to construct a tuple given an attribute and a pair of elements") {
          val attributes = Type.tuple(stringType, intType)
          val element1   = string(stringType, "Scala")
          val element2   = int(intType, 3)
          val actual     = tuple(attributes, element1, element2)
          assertTrue(
            actual == Tuple(attributes, Chunk(element1, element2)),
            actual.attributes == attributes,
            actual.toString() == "(\"Scala\", 3)"
          )
        },
        test("It should be possible to construct a tuple given an attribute and many elements") {
          val attributes = Type.tuple(stringType, intType, boolType)
          val element1   = string(stringType, "John Doe")
          val element2   = int(intType, 42)
          val element3   = boolean(boolType, true)
          val actual     = tuple(attributes, element1, element2, element3)
          assertTrue(
            actual == Tuple(attributes, Chunk(element1, element2, element3)),
            actual == tuple(attributes, Chunk(element1, element2, element3)),
            actual.attributes == attributes,
            actual.toString() == "(\"John Doe\", 42, True)"
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct an empty (un-attributed) tuple") {
          val actual = tuple()
          assertTrue(
            actual == Tuple.Raw(Chunk.empty),
            actual == Tuple((), Chunk.empty),
            actual == Tuple.Raw(),
            actual.attributes == (),
            actual.toString() == "()"
          )
        },
        test("It should be possible to construct a (un-attributed) single element tuple") {
          val element = string("Hello")
          val actual  = tuple(element)
          assertTrue(
            actual == Tuple.Raw(Chunk(element)),
            actual == Tuple((), Chunk(element)),
            actual == Tuple.Raw(element),
            actual == Tuple.Raw(Chunk(element)),
            actual.attributes == (),
            actual.toString() == "(\"Hello\")"
          )
        },
        test("It should be possible to construct a (un-attributed) pair of elements tuple") {
          val element1 = string("Hello")
          val element2 = int(42)
          val actual   = tuple(element1, element2)
          assertTrue(
            actual == Tuple.Raw(Chunk(element1, element2)),
            actual == Tuple((), Chunk(element1, element2)),
            actual == Tuple.Raw(element1, element2),
            actual == Tuple.Raw(Chunk(element1, element2)),
            actual.attributes == (),
            actual.toString() == "(\"Hello\", 42)"
          )
        }
      )
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
              case Variable(attributes, Name.VariableName("alpha")) if attributes == stringType => true
              case _                                                                            => false
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
            actual.foldLeft(Chunk.empty[RawValue])((acc, v) => v +: acc) == Chunk(actual)
          )
        }
      )
    )
  )
}
