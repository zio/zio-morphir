package zio.morphir.json

import zio.{Tag, ZEnvironment}
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write
import zio.morphir.ir._
import zio.morphir.ir.TypeModule.{Field, Type, TypeCase}
import zio.morphir.ir.ValueModule.ValueCase.UnitCase
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.json._
import zio.morphir.json.MorphirJsonCodecV1._
import zio.test._
import zio.test.DefaultRunnableSpec
import zio.test.TestAspect

object EncodingSpec extends DefaultRunnableSpec {
  def spec = suite("encoding")(
    suite("Unit")(
      test("will encode a Unit") {
        val actual   = ()
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Name")(
      test("will encode an empty Name") {
        val actual   = Name.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a single Name") {
        val actual   = Name("Hello")
        val expected = """["hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name") {
        val actual   = Name("HelloThere")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromString") {
        val actual   = Name.fromString("Hello.There")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromList") {
        val actual   = Name.fromList(List("This", "is", "a", "list"))
        val expected = """["this","is","a","list"]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Path")(
      test("will encode an empty Path") {
        val actual   = Path.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = Path.fromString("org")
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = Path.fromString("org.foo.bar")
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModulePath")(
      test("will encode an empty Path") {
        val actual   = ModulePath(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = ModulePath(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = ModulePath(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("PackageName")(
      test("will encode an empty Path") {
        val actual   = PackageName(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = PackageName(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = PackageName(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModuleName")(
      test("will encode an empty ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
        val expected = """[[["org"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
        val expected = """[[["src"],["test"],["scala"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("QName")(
      test("will encode an empty QName") {
        val actual   = QName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a QName") {
        val actual   = QName.fromString("Proper.Path:name")
        val expected = """[[["proper"],["path"]],["name"]]"""
        assertTrue(actual.get.toJson == expected)
      }
    ),
    suite("FQName")(
      test("will encode an empty FQName") {
        val actual   = FQName(Path.empty, Path.empty, Name.empty)
        val expected = "[[],[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a FQName") {
        val actual   = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val expected = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Literal")(
      test("will encode a Literal.Bool") {
        val actual   = Literal.Bool(true)
        val expected = """["bool_literal",true]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.Char") {
        val actual   = Literal.Char('x')
        val expected = """["char_literal","x"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.Float") {
        val actual   = Literal.Float(new java.math.BigDecimal("1.3232"))
        val expected = """["float_literal",1.3232]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.String") {
        val actual   = Literal.String("hello")
        val expected = """["string_literal","hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode an Literal.WholeNumber") {
        val actual   = Literal.WholeNumber(new java.math.BigInteger("321321"))
        val expected = """["int_literal",321321]"""
        assertTrue(actual.toJson == expected)
      }
    )
  )
}
