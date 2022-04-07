package zio.morphir.ir.types
import TypeExpr.FieldT
import zio.morphir.ir.Name

trait FieldSyntax {
  final def defineField(name: Name, fieldType: UType): zio.morphir.ir.types.Field[UType] =
    zio.morphir.ir.types.Field(name, fieldType)

  final def defineField(name: String, fieldType: UType): zio.morphir.ir.types.Field[UType] =
    zio.morphir.ir.types.Field(Name.fromString(name), fieldType)

  final def field[A](name: String, tpe: TypeExpr[A]): FieldT[A] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: TypeExpr[A]): FieldT[A]   = Field(name, tpe)

  final def field[A](name: String, tpe: Type[A]): Field[Type[A]] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): Field[Type[A]]   = Field(name, tpe)

  final def field[A](tuple: (String, TypeExpr[A])): FieldT[A] = Field(Name.fromString(tuple._1), tuple._2)

}
