package cymbium.q2

import scala.language.higherKinds

trait ExtendToQ2[T,A,+BaseAlgebraic[_]] {
  //type BaseAlgebraic[_]

  implicit val q2Like: Q2Like[T,A]
  implicit val baseAlgebraic: BaseAlgebraic[A]
}
