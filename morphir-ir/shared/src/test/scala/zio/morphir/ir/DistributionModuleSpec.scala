package zio.morphir.ir

import zio.morphir.ir
import zio.Chunk
import zio.morphir.ir.DistributionModule.Distribution.Library
import zio.morphir.ir.TypeModule.Definition.TypeAlias
import zio.morphir.ir.TypeModule.Type
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.samples.ModuleExample._
import zio.test._
import zio.morphir.ir.ValueModule.{InputParameter, Value}
import zio.morphir.ir.TypeModule.Type._
import zio.morphir.ir.TypeModule.TypeCase.{TupleCase, VariableCase}
import zio.morphir.ir.ValueModule.ValueCase.UnitCase
import zio.morphir.syntax.AllSyntax

object DistributionModuleSpec extends MorphirBaseSpec with AllSyntax {

  val annotation: UType = defineVariable(Name("variable"))

  val uTypeAlias: Documented[TypeModule.Definition[UType]] = Documented(
    "doc",
    TypeAlias(
      Chunk(Name.fromString("hello")),
      Type(
        VariableCase(Name("world")),
        annotation
      )
    )
  )

  val uTypesMap: Map[Name, AccessControlled[Documented[TypeModule.Definition[UType]]]] = Map {
    Name("hello") -> AccessControlled.publicAccess(uTypeAlias)
  }

//  val x: Type[UType] = Type(TupleCase(Chunk(annotation)), annotation)
//
  val y: Chunk[InputParameter[UType]] = Chunk(
    InputParameter[UType](
      Name("Name"),
      x,
      annotation
    )
  )

  val z: Value[UType] = Value(UnitCase, annotation)

  val ddddd: ValueModule.Definition[Value[UType], UType] = ValueModule.Definition(
    Chunk[UType],
    annotation,
    z
  )

//  val uValuesMap: Map[Name, AccessControlled[ValueModule.ValueDefinition[UType]]] = Map {
//    Name("val") -> AccessControlled.privateAccess(
//
//    )
//  }

//  val uModDef: ModuleDefinition[UType] = ModuleModule.Definition(uTypesMap, uValuesMap)

//  val uPackDef = Map {
//  ModuleName(Path.fromString("java.util"), Name("UUID"))
//      AccessControlled.privateAccess(uModDef)
//  }

//  val library = Library(
//    PackageName(Path.fromString("zio.morphir")),
//    Map {
//      PackageName(Path.fromString("java.util.Arrays")) -> packageSpec
//    },
//    PackageDefinition(Map.empty)
//  )

  def spec = suite("Distribution")(
    test("can look up module specification") {
      assertTrue(1 == 1)
    }
  )
}
