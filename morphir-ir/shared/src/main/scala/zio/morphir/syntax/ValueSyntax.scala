package zio.morphir.syntax

import zio.morphir.ir.value.recursive.AllValueSyntax

trait ValueSyntax extends AllValueSyntax {
  final val types = zio.morphir.syntax.types
}


