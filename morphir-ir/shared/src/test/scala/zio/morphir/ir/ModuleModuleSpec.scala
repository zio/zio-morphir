package zio.morphir.ir

import zio.morphir.ir.ModuleModule.{Definition, Specification}
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.TypeModule.Definition.{CustomType, TypeAlias}
import zio.morphir.ir.TypeModule.Specification.OpaqueTypeSpecification
import zio.morphir.ir.TypeModule.{Constructors, variable}
import zio.morphir.syntax.AllSyntax
import zio.morphir.testing.MorphirBaseSpec
import zio.test.*

object ModuleModuleSpec extends MorphirBaseSpec with AllSyntax {
  val items = Map {
    Name("type")    -> Chunk((Name("var"), defineVariable("var1")))
    Name("rainbow") -> Chunk((Name("red"), defineVariable("red")))
  }

  val definitionTypes = Map {
    Name("hello") -> AccessControlled.privateAccess(
      Documented(
        "doc",
        TypeAlias(Chunk(Name.fromString("hello")), defineVariable("type1"))
      )
    )
    Name("world") -> AccessControlled.privateAccess(
      Documented(
        "doc",
        CustomType(Chunk(Name.fromString("world")), AccessControlled.privateAccess(Constructors(items)))
      )
    )
  }

  val definitionValues = Map {
    Name("val") -> AccessControlled.privateAccess(
      ValueModule.Definition.fromLiteral(string("string"))
    )
  }

  val moduleDef = Definition(definitionTypes, definitionValues)

  val specTypes = Map {
    Name("hello") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name1")), ZEnvironment.empty)
    )
    Name("world") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name2")), ZEnvironment.empty)
    )
  }

  val specValues = Map {
    Name("spec1") -> ValueModule.Specification(
      Chunk(
        (Name("type1"), defineVariable("Float")),
        (Name("type2"), defineVariable("Decimal"))
      ),
      defineVariable("WholeNumbers")
    )
  }

  val moduleSpec = Specification(specTypes, specValues)

  def spec = suite("Type")(
    suite("Module Definition")(
      test("Can be turned to Specification") {
        assertTrue(1 == 1)
      }
    )
  )
}
