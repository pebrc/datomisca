package datomisca
import scala.language.implicitConversions
/**
  * Created by p_brc on 06/08/2016.
  */
package object query {
  implicit def mapI[A,B](l: Seq[A])(implicit conv: A => B): Seq[B] = l.map(conv)
  implicit def symbolToVar(sym: Symbol):Variable = new Variable(sym)
  implicit def findElToFindRel[E1, E <: FindElem](es: Seq[E1])(implicit ev: E1 => E): FindRel = {
    FindRel(es.map(ev))
  }

}
