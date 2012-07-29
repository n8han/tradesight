package com.novus.tradesight

import unfiltered.request._
import unfiltered.response._
import org.joda.time.DateTime
import collection.SortedMap

/*
 * Notes
 * 
 * ReturnsIndex is defined in the package object as:
 *   type ReturnsIndex = Map[String, collection.SortedMap[org.joda.time.DateTime, Double]]
 */
object ProblemsPlan {
  def apply(streamMap: ReturnsIndex) =
    unfiltered.filter.Planify {
      case GET(Path("/weighted")) =>
        val stream = 
          NamedStream("equal weighted portfolio",
            weightedReturns(streamMap).map(dataPoint)
        )
        Json(anyJson(stream :: Nil))
    }

  def weightedReturns(streamMap: ReturnsIndex):
  SortedMap[DateTime, Double] = {
    val interesting = for (vls <- streamMap.values; vl <- vls) yield vl
    (SortedMap.empty[DateTime, Double] /: interesting) {
      case (acc, (date, db)) =>
        acc + (date -> (acc.get(date).getOrElse(0.0) + db))
    }
  }

}
