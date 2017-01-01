package datomisca.query




/**
  * Created by p_brc on 06/08/2016.
  */


case class Find(spec: FindSpec, whereClauses: Option[WhereClauses] = None)  {
  def `with`(withClause: WithClause) = this
  def where(whereClauses: WhereClauses) = this.copy(whereClauses = Some(whereClauses))
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

case class Variable(val sym:Symbol) extends FindElem
trait SrcVar
case object $db extends SrcVar

case class WithClause(vars: Seq[String])
case class Inputs(in: Seq[String])
case class WhereClauses(clauses: Seq[Clause])

trait Clause
trait ExpressionClause extends Clause

case class DataPattern(pattern: Seq[DataTerm], srcVar:Option[SrcVar] = None ) extends ExpressionClause {
  def ::(t:DataTerm) = this.copy(pattern = t +: this.pattern)
}


