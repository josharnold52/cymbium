package cymbium.newq2.syntax
import cymbium.newq2.LinearRepr
import scala.language.experimental.macros
import scala.language.implicitConversions

class LinearReprSyntax {
  implicit def lrepr[T,A](a1: A, a2: A)(implicit ev: LinearRepr[T,A]): T =
    macro cymbium.macros.DefaultForwarders.forwardOf2[A,A, LinearRepr[T,A], T] //ev.of(x,y)

  implicit def toLinearReprOps[S,A](x: S)(implicit ev: LinearRepr[S,A]): LinearRepr.Ops[S,A]
    = new LinearRepr.Ops[S,A](x)
}
