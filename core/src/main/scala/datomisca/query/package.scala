package datomisca
import scala.language.implicitConversions

import shapeless._
/**
  *
  * Created by p_brc on 06/08/2016.
  */
package object query {
  implicit def mapI[A,B](l: Seq[A])(implicit conv: A => B): Seq[B] = l.map(conv)
  implicit def symbolToVar(sym: Symbol):Variable = new Variable(sym)
  implicit def findElToFindRel[E1, E <: FindElem](es: Seq[E1])(implicit ev: E1 => E): FindRel = {
    FindRel(es.map(ev))
  }

  implicit def varToTerm[V](v: V)(implicit ev: V =>  Variable): DataTerm = Coproduct[DataTerm](ev.apply(v))
  implicit def kwToTerm(kw: Keyword): DataTerm = Coproduct[DataTerm](kw)

  type DataTerm = Variable :+:  Keyword :+: String :+: Int :+: Long :+: CNil

  implicit def clausesToWhere(cs: Seq[Clause]):WhereClauses = WhereClauses(cs)

  implicit class DataPatternBuilder(srcVar: SrcVar) {
    def %:[T](t:T)(implicit conf: T => DataTerm) = DataPattern(Seq(t), Some(srcVar))
  }



}
