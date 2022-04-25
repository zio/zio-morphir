package zio.morphir.ir.module

import zio.morphir.ir.Type.{Specification => TypeSpecification}
import zio.morphir.ir.Value.{Specification => ValueSpecification}
import zio.morphir.ir.{Documented, Name}

final case class Specification[+TA](
    types: Map[Name, Documented[TypeSpecification[TA]]],
    values: Map[Name, Documented[ValueSpecification[TA]]]
) { self =>

  def eraseAttributes: Specification[Any] = mapAttributes(_ => ())

  /** Look up the type specification by its name. */
  def lookupType(localName: Name): Option[TypeSpecification[TA]] =
    types.get(localName).map(doc => doc.value)

  /** Look up a value specification by its name. */
  def lookupValue(localName: Name): Option[ValueSpecification[TA]] =
    values.get(localName).map(_.value)

  @inline def map[TB](f: TA => TB): Specification[TB] = mapAttributes(f)

  def mapAttributes[TB](f: TA => TB): Specification[TB] = {
    val types = self.types.map { case (name, doc) =>
      name -> doc.map(_.map(f))
    }

    val values = self.values.map { case (name, doc) =>
      name -> doc.map(_.map(f))
    }

    Specification(types, values)
  }
}

object Specification {
  def empty[TA]: Specification[TA] = Specification[TA](Map.empty, Map.empty)

  type Raw = Specification[Any]
  object Raw {
    def apply(
        types: Map[Name, Documented[TypeSpecification[Any]]],
        values: Map[Name, Documented[ValueSpecification[Any]]]
    ): Raw = Specification(types, values)
  }
}
