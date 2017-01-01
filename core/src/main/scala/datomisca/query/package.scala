package datomisca
import scala.language.implicitConversions
import shapeless._
import java.{util => ju}

/**
  *
  * Created by p_brc on 06/08/2016.
  */
package object query {

  object dataTermToDatomic extends Poly1 {
    implicit def caseVariable = at[Variable](v => findElemCast.to(v))
    implicit def caseKeyword = at[Keyword](identity)
    implicit def caseString = at[String](identity)
    implicit def caseInt = at[Int]((identity[Int] _).andThen(_.asInstanceOf[AnyRef]))
    implicit def caseLong = at[Long]((identity[Long] _).andThen(_.asInstanceOf[AnyRef]))
  }

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

  implicit def whereClausesToDatomic: ToDatomic[ju.List[_], WhereClauses] = {
    ToDatomic[ju.List[_], WhereClauses]{
      (w:WhereClauses) => {
        val builder = Seq.newBuilder[AnyRef]
        builder += Datomic.KW(":where")
        w.clauses.foreach {
          case DataPattern(terms, _) =>
            val pattern = Seq.newBuilder[AnyRef]
            terms.map(_.map(dataTermToDatomic).unify).foreach(pattern += _)
            builder += datomic.Util.list(pattern.result: _*)
        }
        datomic.Util.list(builder.result: _*)
      }
    }
  }

  implicit def findSpecToDatomic(implicit conv: ToDatomicCast[FindElem]): ToDatomic[ju.List[_], FindSpec] = {
    ToDatomic[ju.List[_], FindSpec]{
      (s:FindSpec)=> {
        val builder = Seq.newBuilder[AnyRef]
        builder += Datomic.KW(":find")
        s match {
          case FindRel(es) => es.foreach(builder += conv.to(_))
          case FindColl(es)  =>
            val collBuilder = Seq.newBuilder[AnyRef]
            es.foreach(collBuilder += _)
            collBuilder += datomic.Util.read("...")
            builder += datomic.Util.list(collBuilder.result: _*)
          case FindTuple(es) => builder += datomic.Util.list(es:_*)
          case FindScalar(e) =>
            builder += e
            builder += datomic.Util.read(".")
        }
        datomic.Util.list(builder.result: _*)
      }
    }
  }

  implicit def findElemCast: ToDatomicCast[FindElem] = ToDatomicCast[FindElem]{
    (f:FindElem) => f match {
      case Variable(sym) => clojure.lang.Symbol.create("?"+sym.name)
      case _ => "unsupported"
    }
  }

  implicit def findCast(implicit conv: ToDatomic[ju.List[_], FindSpec], wconv: ToDatomic[ju.List[_], WhereClauses]):ToDatomicCast[Find] = ToDatomicCast[Find]{
    (f:Find) => {
      val wheres: ju.List[_] = f.whereClauses.map(wconv.to).getOrElse(ju.Collections.emptyList())
      datomic.Util.list(conv.to(f.spec).toArray ++ wheres.toArray: _*)
    }
  }


}
