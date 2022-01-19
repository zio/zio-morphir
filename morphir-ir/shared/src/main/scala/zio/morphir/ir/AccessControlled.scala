package zio.morphir.ir

import AccessControlled.Access

final case class AccessControlled[+A](access: Access, value: A) { self =>
  def map[B](f: A => B): AccessControlled[B] =
    AccessControlled(access, f(value))

  def flatMap[B](f: A => AccessControlled[B]): AccessControlled[B] = {
    f(value)
  }
}

object AccessControlled {

  def publicAccess[A](value: A): AccessControlled[A]  = AccessControlled(Access.Public, value)
  def privateAccess[A](value: A): AccessControlled[A] = AccessControlled(Access.Private, value)

  sealed trait Access
  object Access {
    case object Public  extends Access
    case object Private extends Access
  }
}
