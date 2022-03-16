package zio.morphir.samples

import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.ModuleModule.{Definition, Specification}
import zio.morphir.ir.ModuleModuleSpec.{defineVariable, string}
import zio.morphir.ir.TypeModule.Constructors
import zio.morphir.ir.TypeModule.Definition.{CustomType, TypeAlias}
import zio.morphir.ir.TypeModule.Specification.OpaqueTypeSpecification
import zio.morphir.ir.{AccessControlled, Documented, Name, ValueModule}

object ModuleExample {
  val items = Map {
    Name("type")    -> Chunk((Name("var"), defineVariable("var1")))
    Name("rainbow") -> Chunk((Name("red"), defineVariable("red")))
  }

  val typeAlias = Documented(
    "doc",
    TypeAlias(Chunk(Name.fromString("hello")), defineVariable("type1"))
  )

  val customType = Documented(
    "doc",
    CustomType(Chunk(Name.fromString("world")), AccessControlled.publicAccess(Constructors(items)))
  )

  val definitionTypes = Map {
    Name("hello") -> AccessControlled.publicAccess(typeAlias)
    Name("world") -> AccessControlled.publicAccess(customType)
  }

  val definitionValues = Map {
    Name("val") -> AccessControlled.publicAccess(
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

}
