package zio.morphir.ir

import zio.Chunk
import zio.morphir.ir
import zio.prelude.*

object naming {
  object Name extends Newtype[Chunk[String]]
  type Name = Name.Type

  final case class Path(segments: Chunk[Name]) extends AnyVal {
    def /(name: Name): Path = Path(segments ++ Chunk(name))
    //def /(name: String): Path = Path(segments ++ Chunk(Name(name)))
    def /(name: Path): Path = Path(segments ++ name.segments)
  }

  final case class PackagePath(toPath: Path)
  final case class ModulePath(toPath: Path)
  final case class FQName(packagePath: PackagePath, modulePath: ModulePath, localName: Name)
}
