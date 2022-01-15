package zio.morphir.io

sealed trait VFilePath { self =>
  import VFilePath.*

  def cwd: VFilePath = self match {
    case AbsolutePath(root :: Nil, _) => Root(fileSystem)
    case AbsolutePath(toList, _)      => ???
    case RelativePath(toList, _)      => ???
    case Root(_)                      => ???
  }

  def fileSystem: VFileSystem

  final def isRooted: Boolean = self match {
    case Root(_) => true
    case _       => false
  }

  final def path: String = toString()

  def toList: List[String]
  final override def toString: String = toList.mkString(fileSystem.fileSeparator)
}

// final case class VFilePath private (private val segments: List[String]) { self =>
//   def /(segment: String): VFilePath  = VFilePath(segment :: segments)
//   def /(child: VFilePath): VFilePath = VFilePath(child.segments ++ segments)

//   def cwd: VFilePath = self match {
//     case VFilePath(Nil)          => self
//     case VFilePath(head :: tail) => VFilePath(tail)
//   }

//   def dirname: VFilePath = self match {
//     case VFilePath(Nil)          => self
//     case VFilePath(head :: tail) => VFilePath(head :: Nil)
//   }

//   def path: String = toString()

//   override def toString: String = segments.reverse.mkString("/")
// }

object VFilePath {
  def root(implicit fileSystem: VFileSystem): Root = Root(fileSystem)
  def apply(path: String)(implicit fs: VFileSystem): VFilePath = {
    path.split(fs.fileSeparator).toList match {
      case Nil                                                         => Root(fs)
      case parts @ (head :: tail) if head.startsWith(fs.fileSeparator) => AbsolutePath(parts, fs)
      case parts @ (head :: tail)                                      => RelativePath(parts, fs)
    }
  }

  final case class AbsolutePath private[VFilePath] (toList: ::[String], fileSystem: VFileSystem) extends VFilePath
  final case class RelativePath private[VFilePath] (toList: ::[String], fileSystem: VFileSystem) extends VFilePath
  final case class Root(fileSystem: VFileSystem) extends VFilePath {
    override def toList: List[String] = Nil
  }

}
