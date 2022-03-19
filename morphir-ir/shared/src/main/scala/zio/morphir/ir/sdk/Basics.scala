package zio.morphir.ir.sdk

import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.TypeModule.Specification.{CustomTypeSpecification, OpaqueTypeSpecification}
import zio.morphir.ir.UType.reference
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.{Module, UType}
import zio.morphir.syntax.NamingSyntax._

object Basics {
  val moduleName: ModuleName = ModuleName.fromString("Basics")
  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Int")   -> OpaqueTypeSpecification() ?? "Type that represents an integer value.",
      name("Float") -> OpaqueTypeSpecification() ?? "Type that represents a floating-point number.",
      name("Order") -> CustomTypeSpecification.mkEnum(
        "LT",
        "EQ",
        "GT"
      ) ?? "Represents the relative ordering of two things. The relations are less than, equal to, and greater than.",
      name("Bool")  -> OpaqueTypeSpecification() ?? "Type that represents a boolean value.",
      name("Never") -> OpaqueTypeSpecification() ?? "A value that can never happen."
    ),
    values = Map(
      // number
      vSpec("add", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("subtract", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("multiply", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("divide", "a" -> floatType, "b" -> floatType)(floatType),
      vSpec("integerDivide", "a" -> intType, "b" -> intType)(intType),
      vSpec("toFloat", "a" -> intType)(floatType),

      // eq
      vSpec("equal", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),
      vSpec("notEqual", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),

      // comparable
      vSpec("compare", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(orderType),

      // Bool
      vSpec("not", "a" -> boolType)(boolType),

      // Break
      vSpec("identity", "a" -> tVar("a"))(tVar("a")),
      vSpec("always", "a" -> tVar("a"), "b" -> tVar("b"))(tVar("a")),
      vSpec("never", "a" -> neverType)(tVar("a"))
    )
  )

  lazy val boolType: UType  = reference((toFQName(moduleName, "Bool")))
  lazy val floatType: UType = reference((toFQName(moduleName, "Float")))
  lazy val intType: UType   = reference((toFQName(moduleName, "Int")))
  lazy val neverType: UType = reference((toFQName(moduleName, "Never")))
  lazy val orderType: UType = reference((toFQName(moduleName, "Order")))
}
