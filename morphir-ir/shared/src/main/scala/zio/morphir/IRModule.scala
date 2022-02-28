package zio.morphir

import zio.Chunk
import zio.morphir.ir.*
import zio.morphir.ir.{TypeModule, ValueModule}
import zio.morphir.ir.TypeModule.UType

object IRModule {

  final case class IR(
      valueSpecifications: Map[FQName, ValueModule.Specification[Any]],
      valueDefinitions: Map[FQName, ValueModule.ValueDefinition[UType]],
      typeSpecifications: Map[FQName, TypeModule.Specification[Any]],
      typeConstructors: Map[FQName, (FQName, Chunk[Name], Chunk[(Name, UType)])]
  ) { self =>

    def resolveAliases(fqName: FQName): FQName =
      typeSpecifications.get(fqName) match {
        case Some(typeSpecification) =>
          typeSpecification match {
            case TypeModule.Specification.TypeAliasSpecification(_, underlyingType, _) =>
              underlyingType.caseValue match {
                case TypeModule.TypeCase.ReferenceCase(fqName, _) =>
                  fqName
                case _ => fqName
              }
            case _ => fqName
          }
        case None => fqName
      }
  }

  object IR {
    val empty: IR = IR(
      valueSpecifications = Map.empty,
      valueDefinitions = Map.empty,
      typeSpecifications = Map.empty,
      typeConstructors = Map.empty
    )

    def fromDistribution(distribution: DistributionModule.Distribution): IR = ???

    def fromPackageSpecifivations(specs: Map[FQName, PackageModule.Specification[Any]]): IR = ???
  }
}
