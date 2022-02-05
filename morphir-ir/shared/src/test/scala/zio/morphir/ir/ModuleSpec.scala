package zio.morphir.ir

import zio.test.*
import zio.morphir.ir.testing.MorphirBaseSpec

object ModuleSpec extends MorphirBaseSpec {
  def spec = suite("Module Spec")(
    suite("Definition")(
      test("It can be empty") {
        assertTrue(
          Module.emptyDefinition == Module.Definition.empty,
          Module.emptyDefinition.types == Map.empty,
          Module.emptyDefinition.values == Map.empty
        )
      }
    ),
    suite("Specification")(
      test("It can be empty") {
        assertTrue(
          Module.emptySpecification == Module.Specification.empty,
          Module.emptySpecification.types == Map.empty,
          Module.emptySpecification.values == Map.empty
        )
      }
    )
  )
}
