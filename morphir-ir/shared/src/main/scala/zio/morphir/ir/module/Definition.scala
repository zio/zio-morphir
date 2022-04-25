package zio.morphir.ir.module

import zio.morphir.ir.{AccessControlled, Documented, FQName, Name, Type, Value}

import Type.Definition.{CustomType, TypeAlias}

/**
 * Type that represents a module definition. A module definition contains all the details including the implementation
 * and private types and values.
 *
 * A module contains types and values which is represented by the fields in this type:
 *   - `types`: a map of local name to access controlled, documented type specification
 *   - `values`: a map of local name to access controlled, documented value specification
 *
 * @param types
 *   a map of local name to access controlled, documented type specification
 * @param values
 *   a map of local name to access controlled, documented value specification
 */
final case class Definition[+TA, +VA](
    types: Map[Name, AccessControlled[Documented[Type.Definition[TA]]]],
    values: Map[Name, AccessControlled[Documented[Value.Definition[TA, VA]]]]
) { self =>
  def toSpecification: Specification[TA] =
    Specification(
      types = self.types.collect { case (name, AccessControlled.WithPublicAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      },
      values = self.values.collect { case (name, AccessControlled.WithPublicAccess(definition)) =>
        name -> definition.map(_.toSpecification)
      }
    )

  def toSpecificationWithPrivate: Specification[TA] =
    Specification(
      types = self.types.collect { case (name, AccessControlled.WithPrivateAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      },
      values = self.values.collect { case (name, AccessControlled.WithPrivateAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      }
    )

  def lookupValueDefinition(localName: Name): Option[Value.Definition[TA, VA]] =
    values.get(localName).map(accessControlled => accessControlled.withPrivateAccess.value)

  def eraseAttributes: Definition[Any, Any] = ???

  def mapAttributes[TB, VB](tf: TA => TB, vf: VA => VB): Definition[TB, VB] = {
    val types = self.types.map { case (name, accessControlled) =>
      name -> accessControlled.map(documented => documented.map(_.map(tf)))
    }

    val values = self.values.map { case (name, accessControlled) =>
      name -> accessControlled.map(_.map(_.mapAttributes(tf, vf)))
    }

    Definition(types, values)
  }

  def collectTypeReferences: Set[FQName] = self.types.flatMap {
    case (_, AccessControlled.WithPrivateAccess(definition)) =>
      definition.value match {
        case TypeAlias(_, typeExp) => typeExp.collectReferences
        case CustomType(_, ctors)  => ctors.withPrivateAccess.collectReferences
      }
    case (_, AccessControlled.WithPublicAccess(definition)) =>
      definition.value match {
        case TypeAlias(_, typeExp) => typeExp.collectReferences
        case CustomType(_, ctors)  => ctors.withPrivateAccess.collectReferences
      }
    case _ => Nil

  }.toSet

  def collectValueReferences: Set[FQName] = self.values.flatMap {
    case (_, AccessControlled.WithPrivateAccess(documented)) =>
      documented.value.body.collectReferences
    case (_, AccessControlled.WithPublicAccess(documented)) =>
      documented.value.body.collectReferences
    case _ => Nil
  }.toSet

  def collectReferences: Set[FQName] = collectTypeReferences ++ collectValueReferences
  def dependsOnModules: Set[QualifiedModuleName] = self.collectReferences.map { case FQName(pp, mp, _) =>
    QualifiedModuleName(pp.toPath, mp.toPath)
  }
}

object Definition {
  def empty[TA, VA]: Definition[TA, VA] = Definition[TA, VA](Map.empty, Map.empty)
}
