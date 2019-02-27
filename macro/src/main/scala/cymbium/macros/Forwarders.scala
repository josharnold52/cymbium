package cymbium.macros

import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds

trait Forwarders {
  /**
    * Given context, this method rewrites the tree to call the desired
    * method with the lhs parameter. We find the symbol which is
    * applying the macro and use its name to determine what method to
    * call.
    *
    * If we see code like:
    *
    * {{{
    *   defQ2(a1, a2)
    * }}}
    *
    * After typing and implicit resolution, we get trees like:
    *
    * {{{
    *   defQ2(a1, a2)(ev): R
    * }}}
    *
    * The macro should produce trees like:
    *
    * {{{
    *   ev.of(a1, a2): R
    * }}}
    *
    * @group macros
    */
  def forwardOf2[A1,A2,Ev, R](c: Context)(a1: c.Expr[A1], a2: c.Expr[A2])(ev: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    //val lhs = unpackWithoutEv(c)
    //c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs)))

    //System.err.println(c.prefix)
    //System.err.println(c.prefix.tree)
    //System.err.println(a1)
    //System.err.println(a2)
    //System.err.println(ev)

    val mName = TermName(of2Name)
    //val mName =

    c.Expr(q"${ev}.$mName($a1,$a2)")
  }

  def of2Name: String

  /*
  def id(c: Context, name: String) = {
    import c.universe._
    Ident(TermName(name))
  }
  */
}

object DefaultForwarders extends Forwarders {
  def of2Name = "of"
}

