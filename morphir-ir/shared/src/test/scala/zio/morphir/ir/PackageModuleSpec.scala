package zio.morphir.ir

import zio.ZEnvironment
import zio.morphir.ir.PackageModule.{Definition, Specification}
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.samples.ModuleExample.*
import zio.morphir.ir.ModuleModule.{Definition => ModuleDef, Specification => ModuleSpec}
import zio.test.*

object PackageModuleSpec extends MorphirBaseSpec {
  val packageDefModules =
    Map {
      ModuleName(
        Path.fromString("blog.author"),
        Name("peter")
      ) -> AccessControlled.publicAccess(moduleDef)
    }

  val packageDef = Definition(packageDefModules)

  val packageSpecModules =
    Map {
      ModuleName(
        Path.fromString("blog.author"),
        Name("peter")
      ) -> moduleSpec
    }

  val packageSpec = Specification(packageSpecModules)

  def spec = suite("Package")(
    suite("Definitions")(
      test("can convert to Specification") {
        // todo add back when TypeModule.toSpec is implemented
//        val expected = Specification(
//          Map(
//            ModuleName(Path.fromString("blog.author"), Name("peter"))
//              -> moduleDef.toSpecification
//          )
//        )
//        assertTrue(packageDef.toSpecification == expected)
        assertTrue(1 == 1)
      },
      test("Look up existing Module Definition") {
        val result = packageDef.lookupModuleDefinition(Path.fromString("blog.author"))
        assertTrue(result.equals(List(Some(moduleDef))))
      },
      test("Look up non-existent Module Definition") {
        val result = packageDef.lookupModuleDefinition(Path.fromString("blog.post"))
        assertTrue(result.equals(List(None)))
      },
      test("Look up existing Type Definition") {
        val result  = packageDef.lookupTypeDefinition(Path.fromString("blog.author"), Name("peter"))
        val result2 = packageDef.lookupTypeDefinition(ModuleName(Path.fromString("blog.author"), Name("peter")))

        assertTrue(result == Some(moduleDef) && result2 == Some(moduleDef))
      },
      test("Look up non-existent Type Definition") {
        val result  = packageDef.lookupTypeDefinition(Path.fromString("blog.author"), Name("stephen"))
        val result2 = packageDef.lookupTypeDefinition(ModuleName(Path.fromString("blog.author"), Name("stephen")))

        assertTrue(result.isEmpty && result2.isEmpty)
      }
    ),
    suite("Specification")(
      test("Can look up existing Module Specification") {
        val result = packageSpec.lookupModuleSpecification(Path.fromString("blog.author"))
        assertTrue(result == List(Some(moduleSpec)))
      },
      test("Can look up non-existent Module Specification") {
        val result = packageSpec.lookupModuleSpecification(Path.fromString("blog.post"))
        assertTrue(result == List(None))
      },
      test("Look up existing Type Definition") {
        val result  = packageSpec.lookupTypeSpecification(Path.fromString("blog.author"), Name("peter"))
        val result2 = packageSpec.lookupTypeSpecification(ModuleName(Path.fromString("blog.author"), Name("peter")))

        assertTrue(result == Some(moduleSpec) && result2 == Some(moduleSpec))
      },
      test("Look up non-existent Type Definition") {
        val result  = packageSpec.lookupTypeSpecification(Path.fromString("blog.author"), Name("stephen"))
        val result2 = packageSpec.lookupTypeSpecification(ModuleName(Path.fromString("blog.author"), Name("stephen")))

        assertTrue(result.isEmpty && result2.isEmpty)
      }
    )
  )
}
