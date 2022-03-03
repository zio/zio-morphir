/*
rule = AddModuleTypeSpec
 */
package zio.morphir.sdk

import zio.morphir.module

@module(namespace = Some("Morphir.SDK")) object Bool {}

@module(namespace = Some("Morphir.SDK"), name = Some("Basics")) object BasicsModule {}

package foo.bar {
  object Foo
}
