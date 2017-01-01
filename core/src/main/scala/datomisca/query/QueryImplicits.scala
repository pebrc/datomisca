package datomisca.query
import clojure.lang.Keyword
import datomisca.query.DataPattern.DataTerm
import shapeless._

import scala.language.implicitConversions
/**
  * Created by p_brc on 01/01/2017.
  */
trait QueryImplicits {
  implicit def mapI[A,B](l: Seq[A])(implicit conv: A => B): Seq[B] = l.map(conv)
  implicit def symbolToVar(sym: Symbol):Variable = Variable(sym)
  implicit def findElToFindRel[E1, E <: FindElem](es: Seq[E1])(implicit ev: E1 => E): FindRel = {
    FindRel(es.map(ev))
  }

  implicit def varToTerm[V](v: V)(implicit ev: V =>  Variable): DataTerm = Coproduct[DataTerm](ev.apply(v))
  implicit def kwToTerm(kw: Keyword): DataTerm = Coproduct[DataTerm](kw)



  implicit def clausesToWhere(cs: Seq[Clause]):WhereClauses = WhereClauses(cs)

  implicit class DataPatternBuilder(srcVar: SrcVar) {
    def %:[T](t:T)(implicit conf: T => DataTerm) = DataPattern(Seq(t), Some(srcVar))
  }
}
