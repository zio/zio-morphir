package zio.morphir.fix

import scalafix.v1.SemanticRule
import com.google.protobuf.Syntax
import scalafix.v1.SemanticDocument
import scalafix.v1.Patch

class AddModuleTypeSpec extends SemanticRule("AddModuleTypeSpec") {
  override def fix(implicit doc: SemanticDocument): Patch = Patch.empty
}
