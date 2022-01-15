package zio.morphir.io

trait VFileSystemPlatformSpecific {
  final class StandardFileSystem extends VFileSystem {
    override def fileSeparator: String = java.io.File.separator
  }
}
