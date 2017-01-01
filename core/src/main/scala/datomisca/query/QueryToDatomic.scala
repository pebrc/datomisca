package datomisca.query

import datomisca._
import shapeless._
import java.{util => ju}


/**
  * Created by p_brc on 01/01/2017.
  */
trait QueryToDatomic {

  object dataTermToDatomic extends Poly1 {
    implicit def caseVariable = at[Variable](v => findElemCast.to(v))
    implicit def caseKeyword = at[Keyword](identity)
    implicit def caseString = at[String](identity)
    implicit def caseInt = at[Int]((identity[Int] _).andThen(_.asInstanceOf[AnyRef]))
    implicit def caseLong = at[Long]((identity[Long] _).andThen(_.asInstanceOf[AnyRef]))
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

  implicit def findCast(implicit conv: ToDatomic[ju.List[_], FindSpec], wconv: ToDatomic[ju.List[_], WhereClauses]):ToDatomicCast[find] = ToDatomicCast[find]{
    (f:find) => {
      val wheres: ju.List[_] = f.whereClauses.map(wconv.to).getOrElse(ju.Collections.emptyList())
      datomic.Util.list(conv.to(f.spec).toArray ++ wheres.toArray: _*)
    }
  }
}
