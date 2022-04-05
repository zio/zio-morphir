package zio.morphir.ir.types

import zio.morphir.ir.Name
import zio.morphir.testing.MorphirBaseSpec
import zio.test._
import TypeCase._
import zio.Chunk

object TypeExprSpec extends MorphirBaseSpec {
  def spec = suite("TypeExpr Spec")(
    suite("Operations")(
      test("Can be documented") {
        import UTypeExpr._
        val actual = variable("a") ?? "Some type variable"
        assertTrue(actual.doc == "Some type variable")
      }
    ),
    suite("Variable")(
      test("testing first variable constructor") {
        import UTypeExpr._
        val actual = variable("FizzBuzz")
        assertTrue(actual.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz")))
      },
      test("testing second variable constructor") {
        import UTypeExpr._
        val actual = variable(Name("FizzBuzz"))
        assertTrue(actual.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz")))
      },
      test("eraseAttributes should clear out the Attributes") {
        val actual   = TypeExpr.variable((0, 0), "foo")
        val expected = UTypeExpr.variable("foo")
        assertTrue(
          actual != expected,
          actual.attributes == ((0, 0)) && expected.attributes == (()),
          actual.eraseAttributes == UTypeExpr.variable("foo"),
          actual.eraseAttributes == actual.mapAttributes(_ => (()))
        )
      }
    ),
    suite("Field")(
      test("testing first field constructor") {
        import UTypeExpr._
        val actual = field(Name("field1"), variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("testing second field constructor") {
        import UTypeExpr._
        val actual = field("field1", variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      }
    ),
    suite("Record")(
      // test("testing first record constructor") {
      //   val var1   = field("first", variable("hello"))
      //   val var2   = field("second", variable("there"))
      //   val chunk  = zio.Chunk(var1, var2)
      //   val actual = record(chunk)
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Record(_, fields) => fields.contains(var1) && fields.contains(var2) }
      //   )
      // },
      // test("testing second record constructor") {
      //   val var1   = field("first", variable("hello"))
      //   val var2   = field("second", variable("there"))
      //   val actual = record(var1, var2)
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Record(_, fields) => fields.contains(var1) && fields.contains(var2) }
      //   )
      // }
    ),
    suite("Tuple")(
      test("testing emptyTuple constructor") {
        import TypeExpr._
        val actual = emptyTuple("FizzBuzz")
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(attributes, fields) => fields.isEmpty && attributes == "FizzBuzz" },
          actual.attributes == "FizzBuzz"
        )
      },
      test("testing tuple constructor when given a chunk") {
        import UTypeExpr._
        val var1   = variable("hello")
        val var2   = variable("there")
        val chunk  = zio.Chunk(var1, var2)
        val actual = tuple(chunk)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(_, elements) => elements.contains(var1) && elements.contains(var2) }
        )
      },
      test("testing tuple constructor when given multiple un-attributed elements") {
        import UTypeExpr._
        val var1   = variable("hello")
        val var2   = variable("there")
        val var3   = variable("notThere")
        val actual = tuple(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(_, elements) =>
            elements.contains(var1) && elements.contains(var2) && !elements.contains(var3)
          },
          actual.attributes == ()
        )
      },
      test("testing tuple with attributes constructor") {
        import TypeExpr._
        val var1   = variable("A", "a")
        val var2   = variable("B", "b")
        val var3   = variable("C", "c")
        val actual = tuple("Tuple3[a,b,c]", var1, var2, var3)
        assertTrue(
          actual.attributes == "Tuple3[a,b,c]",
          actual.satisfiesCaseOf { case TupleCase(_, elements) => elements == Chunk(var1, var2, var3) }
        )
      }
    ),
    suite("Function")(
//      test("testing first function constructor") {
//        val param1  = variable("v1")
//        val param2  = variable("v2")
//        val retType = tuple(variable("v3"), variable("v4"))
//        val actual  = function(zio.Chunk(param1, param2), retType)
//        assertTrue(
//          actual.satisfiesCaseOf { case FunctionCase(_, params, returnType) =>
//            params.contains(param1) && params.contains(param2) && returnType == retType
//          }
//        )
//      },
//      test("testing second function constructor") {
//        val param1  = variable("v1")
//        val param2  = variable("v2")
//        val retType = tuple(variable("v3"), variable("v4"))
//        val actual  = function(param1, param2)(retType, ())
//        assertTrue(
//          actual.satisfiesCaseOf { case FunctionCase(_, params, returnType) =>
//            params.contains(param1) && params.contains(param2) && returnType == retType
//          }
//        )
//      }
    ),
    suite("Extensible Record")(
      test("testing first extensible record constructor") {
        import UTypeExpr._
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecord(n1, zio.Chunk(f1, f2, f3))
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name == n1 && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing second extensible record constructor") {
        import UTypeExpr._
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecordWithFields(n1, f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name == n1 && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing third extensible record constructor") {
        import UTypeExpr._
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecord("SomeName", zio.Chunk(f1, f2, f3))
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing fourth extensible record constructor") {
        import UTypeExpr._
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecordWithFields("SomeName", f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      }
    ),
    suite("Reference")(
      // test("testing first reference constructor") {
      //   val v1     = variable("v1")
      //   val v2     = variable("v2")
      //   val v3     = tuple(variable("v3"), variable("v4"))
      //   val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
      //   val actual = reference(fqn1, zio.Chunk(v1, v2, v3))
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Reference(_, fqName, typeParams) =>
      //       fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
      //     }
      //   )
      // },
      // test("testing second reference constructor") {
      //   val v1     = variable("v1")
      //   val v2     = variable("v2")
      //   val v3     = tuple(variable("v3"), variable("v4"))
      //   val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
      //   val actual = reference(fqn1, v1, v2, v3)
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Reference(_, fqName, typeParams) =>
      //       fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
      //     }
      //   )
      // },
      // test("testing third reference constructor") {
      //   val v1     = variable("v1")
      //   val v2     = variable("v2")
      //   val v3     = tuple(variable("v3"), variable("v4"))
      //   val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
      //   val actual = reference("packageName", "moduleName", "localName", zio.Chunk(v1, v2, v3))
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Reference(_, fqName, typeParams) =>
      //       fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
      //     }
      //   )
      // },
      // test("testing fourth reference constructor") {
      //   val v1     = variable("v1")
      //   val v2     = variable("v2")
      //   val v3     = tuple(variable("v3"), variable("v4"))
      //   val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
      //   val actual = reference("packageName", "moduleName", "localName", v1, v2, v3)
      //   assertTrue(
      //     actual.satisfiesCaseOf { case Reference(_, fqName, typeParams) =>
      //       fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
      //     }
      //   )
      // }
    ),
    suite("Constructors")(
      // test("Can make type constructors for an enum") {
      //   val actual        = Constructors.forEnum("Red", "Yellow", "Green")
      //   val expectedNames = Set("Red", "Yellow", "Green").map(Name.fromString)
      //   assertTrue(
      //     actual.ctorNames == expectedNames,
      //     actual.toMap.values.forall(_.isEmpty)
      //   )
      // }
    )
  )
}
