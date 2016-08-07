package datomisca.query

import java.{util => ju}

import datomisca.{QueryException, QueryExecutor, QueryProcessingException, QueryResultToTuple}

import scala.concurrent._
import scala.util.control.NonFatal

/**
  * Created by p_brc on 07/08/2016.
  */
private[datomisca] trait UntypedQueryExecutor {
  self: QueryExecutor =>

  private[datomisca] def query[OutArgs](q: find, in: AnyRef*)(implicit outConv: QueryResultToTuple[OutArgs]): Iterable[OutArgs] = {
    new Iterable[OutArgs] {
      private val jColl: ju.Collection[ju.List[AnyRef]] = runQuery(q, in)
      override def isEmpty = jColl.isEmpty
      override def size = jColl.size
      override def iterator = new Iterator[OutArgs] {
        private val jIter: ju.Iterator[ju.List[AnyRef]] = jColl.iterator
        override def hasNext = jIter.hasNext
        override def next() = outConv.toTuple(jIter.next())
      }
    }
  }

  def runQuery[OutArgs](q: find, input: Seq[AnyRef]) = {
    val q = datomic.Util.readAll(new java.io.StringReader(find.toString())).asInstanceOf[java.util.List[AnyRef]]
    try {
      blocking {
        datomic.Peer.q(q, input: _*)
      }
    } catch {
      case ex: Throwable if ex.getMessage startsWith "processing" =>
        val builder = Seq.newBuilder[String]
        var e = ex
        while (e.getCause != null) {
          builder += e.getMessage
          e = e.getCause
        }
        throw new QueryProcessingException(e, builder.result)
      case NonFatal(ex) =>
        throw new QueryException(ex)
    }

  }


}
