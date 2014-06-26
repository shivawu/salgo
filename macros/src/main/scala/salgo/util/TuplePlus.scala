package salgo.util

import scala.language.higherKinds

trait TuplePlus[F[_]] {
  def plus[A, B](a: F[A], b: F[B]): F[(A, B)]
}
