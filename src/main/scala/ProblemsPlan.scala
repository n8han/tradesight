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
  def apply(streamMap: ReturnsIndex) = {
    unfiltered.filter.Planify {
      case GET(Path("/weighted")) =>
        val weightedMap = weightedReturns(streamMap)
        val weighted = 
          NamedStream("equal weighted portfolio",
            weightedMap.map(dataPoint)
        )
        val cumMap = cumulativeReturns(weightedMap)
        val cumulative =
          NamedStream("cumulative returns",
            cumMap.map(dataPoint)
        )
        val ds = deltas(cumMap)
        println("best " + ds.maxBy { _.change })
        println("worst " + ds.minBy { _.change })
        Json(anyJson(weighted :: cumulative :: Nil))
    }
  }

  def weightedReturns(streamMap: ReturnsIndex):
  SortedMap[DateTime, Double] = {
    val interesting = for (vls <- streamMap.values; vl <- vls) yield vl
    (SortedMap.empty[DateTime, Double] /: interesting) {
      case (acc, (date, db)) =>
        acc + (date -> (acc.get(date).getOrElse(0.0) + db))
    }
  }
  def cumulativeReturns(weightedMap: SortedMap[DateTime, Double]):
  SortedMap[DateTime, Double] = {
    (SortedMap.empty[DateTime, Double] /: weightedMap) {
      case (acc, (date, db)) =>
        acc + (date -> (
          (for ((dt, vl) <- acc.lastOption) yield vl).getOrElse(0.0) +
            db))
    }
  }
  def deltas(cumMap: SortedMap[DateTime, Double]) = {
    for {
      start <- cumMap.keys
      end <- cumMap.keys if end.isAfter(start)
    } yield {
      Range(start, end, cumMap(end) - cumMap(start))
    }
  }
}
case class Range(start: DateTime, end: DateTime, change: Double)
