package datomisca.query


/**
  * Created by p_brc on 06/08/2016.
  */

case class find(spec: FindSpec) {
  def `with`(withClause: WithClause) = this
  def where(whereClauses: WhereClauses) = this
  def inputs(inputs: Inputs) = this
}

trait FindSpec
case class FindRel(es: Seq[FindElem]) extends FindSpec
case class FindColl(es: Seq[FindElem]) extends FindSpec
case class FindTuple(es: Seq[FindElem]) extends FindSpec
case class FindScalar(e: FindElem) extends FindSpec

trait FindElem

case object PullExpr extends FindElem
case object Aggregate extends FindElem
class Variable(val sym:Symbol) extends FindElem


case class WithClause(vars: Seq[String])
case class Inputs(in: Seq[String])
case class WhereClauses(clauses: Seq[String])


