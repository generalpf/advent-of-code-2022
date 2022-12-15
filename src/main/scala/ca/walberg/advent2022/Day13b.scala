package ca.walberg.advent2022

import grapple.json.{Json, JsonArray, JsonNumber, JsonValue}

import scala.None
import scala.io.Source

@main def Day13b(args: String*): Unit = {
  def isSorted(left: JsonArray, right: JsonArray): Option[Boolean] =
    (0 until left.size).foreach(i =>
      if (i >= right.size)
        return Some(false)
      (left(i), right(i)) match
        case (JsonNumber(l), JsonNumber(r)) if l > r => return Some(false)
        case (JsonNumber(l), JsonNumber(r)) if r > l => return Some(true)
        case (JsonNumber(l), JsonArray(r)) =>
          val is = isSorted(JsonArray(Seq(JsonNumber(l))), JsonArray(r))
          if (is.isDefined) return is
        case (JsonArray(l), JsonNumber(r)) =>
          val is = isSorted(JsonArray(l), JsonArray(Seq(JsonNumber(r))))
          if (is.isDefined) return is
        case (JsonArray(l), JsonArray(r)) =>
          val is = isSorted(JsonArray(l), JsonArray(r))
          if (is.isDefined) return is
        case _ =>
    )
    if (left.size == right.size) None else Some(left.size < right.size)

  val source = Source.fromFile("data/day13.txt")
  val sorted = source.getLines()
    .filter(_ != "")
    .toSeq
    .appended("[[2]]")
    .appended("[[6]]")
    .map(line => Json.parse(line).asInstanceOf[JsonArray])
    .sortWith((left, right) => isSorted(left, right).getOrElse(false))

  val twoIndex = sorted.indexWhere(b =>
    b.size == 1 && (b(0) match
      case JsonArray(a) if a.size == 1 && a(0) == JsonNumber(2) => true
      case _ => false
      )) + 1
  val sixIndex = sorted.indexWhere(b =>
    b.size == 1 && (b(0) match
      case JsonArray(a) if a.size == 1 && a(0) == JsonNumber(6) => true
      case _ => false
      )) + 1

  println(twoIndex * sixIndex)
  source.close()
}
