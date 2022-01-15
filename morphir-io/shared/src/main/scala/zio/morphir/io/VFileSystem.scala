package zio.morphir.io

/**
 * A `VFileSystem` provides a common environment for filesystem like operations.
 */
trait VFileSystem {
  def fileSeparator: String
}

object VFileSystem extends VFileSystemPlatformSpecific with VFileSystemInstances {}

trait VFileSystemInstances { self: VFileSystem.type =>
  implicit val defaultVFileSystem: VFileSystem = new StandardFileSystem
}
