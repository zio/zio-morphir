package zio.morphir
import zio.prelude.AnyType

import zio.Chunk
import zio.morphir.ir.{DistributionModule, FQName, Name, PackageModule, Value}
import zio.morphir.ir.types.UType
import zio.morphir.IR.TypeConstructorInfo
import zio.morphir.ir.Type.{Type, Specification}
import Value.{Definition => VDefinition, Specification => VSpecification}

final case class IR[+Caps[_]](
    valueSpecifications: Map[FQName, VSpecification[Any]],
    valueDefinitions: Map[FQName, VDefinition[Caps, Any, UType]],
    typeSpecifications: Map[FQName, Specification[Any]],
    typeConstructors: Map[FQName, TypeConstructorInfo]
) { self =>

  @inline final def lookupTypeSpecification(fqName: FQName): Option[Specification[Any]] =
    typeSpecifications.get(fqName)

  @inline final def lookupTypeConstructor(fqName: FQName): Option[TypeConstructorInfo] =
    typeConstructors.get(fqName)

  def resolveAliases(fqName: FQName): FQName =
    typeSpecifications.get(fqName) match {
      case Some(typeSpecification) =>
        typeSpecification match {
          case Specification.TypeAliasSpecification(_, underlyingType) =>
            underlyingType match {
              case Type.Reference(_, fqName, _) =>
                fqName
              case _ => fqName
            }
          case _ => fqName
        }
      case None => fqName
    }
}
object IR {

  @inline final def fromDistribution[Caps[_]](distribution: DistributionModule.Distribution): IR[Caps] = ???

  @inline final def fromPackageSpecifications[Caps[_]](specs: Map[FQName, PackageModule.Specification[Any]]): IR[Caps] =
    ???

  @inline final def lookupTypeSpecification[Caps[_]](fqName: FQName): IR.LookupTypeSpecification[Caps] =
    IR.lookupTypeSpecification(fqName)

  @inline final def lookupTypeConstructor[Caps[_]](fqName: FQName): IR.LookupTypeConstructor[Caps] =
    IR.lookupTypeConstructor(fqName)

  val empty: IR[AnyType] = IR(
    valueSpecifications = Map.empty,
    valueDefinitions = Map.empty,
    typeSpecifications = Map.empty,
    typeConstructors = Map.empty
  )

  final class LookupTypeSpecification[Caps[_]](val fqName: () => FQName) extends AnyVal {
    def apply(ir: IR[Caps]): Option[Specification[Any]] =
      ir.lookupTypeSpecification(fqName())
  }

  final class LookupTypeConstructor[+Caps[_]](val fqName: () => FQName) extends AnyVal {
    def apply(ir: IR[Caps]): Option[TypeConstructorInfo] =
      ir.lookupTypeConstructor(fqName())
  }
  final case class TypeConstructorInfo(containingType: FQName, typeParams: Chunk[Name], typeArgs: Chunk[(Name, UType)])
}
