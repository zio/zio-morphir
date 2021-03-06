package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Type.unit
import zio.morphir.ir.Value.Specification
import zio.morphir.ir.{Documented, Module, Name}
import zio.morphir.syntax.NamingSyntax._

object Regex {
  val moduleName: ModuleName = ModuleName.fromString("Regex")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map.empty,
    values = toSpec(
      Chunk(
        "fromString",
        "fromStringWith",
        "never",
        "contains",
        "split",
        "find",
        "replace",
        "splitAtMost",
        "findAtMost",
        "replaceAtMost"
      )
    )
  )

  private def toSpec(values: Chunk[String]): Map[Name, Documented[Specification[Any]]] =
    values.map(valueName => (name(valueName), Documented("", Specification(Chunk.empty, unit)))).toMap
}
