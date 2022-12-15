package ca.walberg.advent2022

import grapple.json.{Json, JsonArray, JsonNumber, JsonValue}

import scala.None
import scala.io.Source

@main def Day13a(args: String*): Unit = {
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
  var pair: Int = 0
  val sum: Int = source.getLines().grouped(3).foldLeft(0)(
    (acc: Int, lines: Seq[String]) =>
      pair += 1
      val left = Json.parse(lines(0)).asInstanceOf[JsonArray]
      val right = Json.parse(lines(1)).asInstanceOf[JsonArray]

      if (isSorted(left, right).getOrElse(false))
        acc + pair
      else
        acc
  )

  println(sum)
  source.close()
}
