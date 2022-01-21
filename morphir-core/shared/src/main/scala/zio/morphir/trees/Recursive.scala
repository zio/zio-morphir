package zio.morphir.trees

import zio.prelude._

final case class Recursive[Case[+_], +Anns](caseValue: Case[Recursive[Case, Anns]], anns: AnnotationMap[Anns]) {

  def fold[Z](f: Case[Z] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.fold(f)))

  def foldAnns[Z](f: (Case[Z], AnnotationMap[Anns]) => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.foldAnns(f)), anns)

//   /**
//    * i Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
//    * structure annotated with the current previous summary values in each step of the fold.
//    */
//   def foldAnnotated[Z](f: Case[Annotated[Case, Z]] => Z)(implicit covariant: Covariant[Case]): Z = {
//     def annotate(recursive: Recursive[Case, Anns]): Annotated[Case, Z] =
//       Annotated(recursive.caseValue.map(annotate), recursive.foldAnnotated(f))
//     f(caseValue.map(annotate))
//   }

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
      foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldRecursive[Z](f: Case[(Recursive[Case, Anns], Z)] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))
}

object Recursive {

  def apply[Case[+_]](caseValue: Case[Recursive[Case, Any]]): Recursive[Case, Any] =
    Recursive(caseValue, AnnotationMap.empty)

  def unfold[Case[+_], Anns, Z](
      z: Z
  )(f: Z => (Case[Z], AnnotationMap[Anns]))(implicit covariant: Covariant[Case]): Recursive[Case, Anns] = {
    val (caseValue, anns) = f(z)
    Recursive(caseValue.map { zed => unfold(zed)(f) }, anns)
  }

  def unfoldRecursive[Case[+_], Anns, Z](z: Z)(
      f: Either[Recursive[Case, Anns], Z] => Case[Either[Recursive[Case, Anns], Z]]
  )(implicit covariant: Covariant[Case]): Recursive[Case, Anns] = {
    // Recursive {
    //   f(Right(z)).map {
    //     case Left(recursive) => recursive
    //     case Right(z)        => unfoldRecursive(z)(f)
    //   }
    // }
    ???
  }
}
