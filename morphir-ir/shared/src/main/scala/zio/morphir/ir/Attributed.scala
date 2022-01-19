package zio.morphir.ir
import AccessControlled.Access

final case class Attributed[Case[+_], A](caseValue: Case[Attributed[Case, A]], attributes: A)



