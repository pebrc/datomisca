package datomisca.query


import clojure.lang.Keyword
import datomisca.query.DataPattern.DataTerm
import datomisca.query.Find.Input
import shapeless._



/**
  * Created by p_brc on 06/08/2016.
  */


case class Find(spec: FindSpec, whereClauses: Option[WhereClauses] = None)  {
  def `with`(withClause: WithClause) = this
  def where(whereClauses: WhereClauses) = this.copy(whereClauses = Some(whereClauses))
  def inputs(inputs: Input*) = this

}
object Find {

  val wPatternVar =  Witness("%")
  type PatternVar = wPatternVar.T
  type Input = SrcVar :+: Variable :+: PatternVar :+: CNil
}

trait FindSpec
case class FindRel(es: Seq[FindElem]) extends FindSpec
case class FindColl(es: Seq[FindElem]) extends FindSpec
case class FindTuple(es: Seq[FindElem]) extends FindSpec
case class FindScalar(e: FindElem) extends FindSpec

trait FindElem

case object PullExpr extends FindElem
case object Aggregate extends FindElem

case class Variable(val sym:Symbol) extends FindElem with Binding

trait SrcVar
case object $db extends SrcVar

case class WithClause(vars: Seq[String])


case class WhereClauses(clauses: Seq[Clause])

trait Clause
trait ExpressionClause extends Clause


case class DataPattern(pattern: Seq[DataTerm], srcVar:Option[SrcVar] = None ) extends ExpressionClause {
  def ::(t:DataTerm) = this.copy(pattern = t +: this.pattern)
}
object DataPattern {
  type DataTerm = Variable :+:  Keyword :+: String :+: Int :+: Long :+: CNil
}

trait Binding
case class BindColl(v:Variable)
case class BindTuple(vs: List[Variable])
case class BindRel(vs: List[Variable])



