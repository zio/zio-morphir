package zio.morphir.io

import zio.test.*
object VFilePathSpec extends DefaultRunnableSpec {
  def spec = suite("VFilePath")(
    test("Creation") {
      val rooted = VFilePath("/foo/bar/baz")
      assertTrue(
        rooted.toString() == "/foo/bar/baz"
      )
    }
  )
}
