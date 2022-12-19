package ca.walberg.advent2022

import scala.collection.mutable
import scala.io.Source

@main def Day18b(args: String*): Unit = {
  val source = Source.fromFile("data/day18.txt")
  val dim = 32
  val lavaCoords: Seq[(Int, Int, Int)] = source.getLines().foldLeft(Seq())(
    (acc: Seq[(Int, Int, Int)], line: String) =>
      val coords = line.split(',').map(_.toInt + 1) // +1 so nothing is on the edge @ 0
      acc :+ (coords(0), coords(1), coords(2))
  )
  source.close()

  def toggleSpaces(space: Array[Array[Array[Boolean]]], toggleThese: Seq[(Int, Int, Int)], setTo: Boolean): Unit =
    toggleThese.foreach((x, y, z) =>
      space(z)(y)(x) = setTo
    )

  def surfaceArea(space: Array[Array[Array[Boolean]]]): Int =
    val bottomUp = space(0).indices
      .flatMap(y => space(0)(y).indices.map(x => (x, y))) // Seq[(x: Int, y: Int)]
      .map((x, y) =>
        space.indices
          .sliding(2) // List((0, 1), (1, 2), ..., (20, 21))
          .count(zPair =>
            space(zPair(0))(y)(x) && !space(zPair(1))(y)(x)
          )
      )
      .sum
    val topDown = space(0).indices
      .flatMap(y => space(0)(y).indices.map(x => (x, y))) // Seq[(x: Int, y: Int)]
      .map((x, y) =>
        space.indices.reverse
          .sliding(2) // List((21, 20), (20, 19), ..., (1, 0))
          .count(zPair =>
            space(zPair(0))(y)(x) && !space(zPair(1))(y)(x)
          )
      )
      .sum
    val fromLeft = space.indices
      .flatMap(z => space(0).indices.map(y => (y, z))) // Seq[(y: Int, z: Int)]
      .map((y, z) =>
        space(0)(0).indices
          .sliding(2) // List((0, 1), (1, 2), ..., (20, 21))
          .count(xPair =>
            space(z)(y)(xPair(0)) && !space(z)(y)(xPair(1))
          )
      )
      .sum
    val fromRight = space.indices
      .flatMap(z => space(0).indices.map(y => (y, z))) // Seq[(y: Int, z: Int)]
      .map((y, z) =>
        space(0)(0).indices.reverse
          .sliding(2) // List((21, 20), (20, 19), ..., (1, 0))
          .count(xPair =>
            space(z)(y)(xPair(0)) && !space(z)(y)(xPair(1))
          )
      )
      .sum
    val fromFront = space(0)(0).indices
      .flatMap(x => space.indices.map(z => (x, z))) // Seq[(x: Int, z: Int)]
      .map((x, z) =>
        space(0).indices
          .sliding(2) // List((0, 1), (1, 2), ..., (20, 21))
          .count(yPair =>
            space(z)(yPair(0))(x) && !space(z)(yPair(1))(x)
          )
      )
      .sum
    val fromBack = space(0)(0).indices
      .flatMap(x => space.indices.map(z => (x, z))) // Seq[(x: Int, z: Int)]
      .map((x, z) =>
        space(0).indices.reverse
          .sliding(2) // List((21, 20), (20, 19), ..., (1, 0))
          .count(yPair =>
            space(z)(yPair(0))(x) && !space(z)(yPair(1))(x)
          )
      )
      .sum
    bottomUp + topDown + fromLeft + fromRight + fromFront + fromBack

  val lavaSpace = Array.fill(dim, dim, dim)(false)
  toggleSpaces(lavaSpace, lavaCoords, true)

  def floodable(from: (Int, Int, Int)): Seq[(Int, Int, Int)] =
    val below: Option[(Int, Int, Int)] = if (from._3 > 0 && !lavaSpace(from._3 - 1)(from._2)(from._1)) Some((from._1, from._2, from._3 - 1)) else None
    val above: Option[(Int, Int, Int)] = if (from._3 < dim - 1 && !lavaSpace(from._3 + 1)(from._2)(from._1)) Some((from._1, from._2, from._3 + 1)) else None
    val inFront: Option[(Int, Int, Int)] = if (from._2 > 0 && !lavaSpace(from._3)(from._2 - 1)(from._1)) Some((from._1, from._2 - 1, from._3)) else None
    val behind: Option[(Int, Int, Int)] = if (from._2 < dim - 1 && !lavaSpace(from._3)(from._2 + 1)(from._1)) Some((from._1, from._2 + 1, from._3)) else None
    val left: Option[(Int, Int, Int)] = if (from._1 > 0 && !lavaSpace(from._3)(from._2)(from._1 - 1)) Some((from._1 - 1, from._2, from._3)) else None
    val right: Option[(Int, Int, Int)] = if (from._1 < dim - 1 && !lavaSpace(from._3)(from._2)(from._1 + 1)) Some((from._1 + 1, from._2, from._3)) else None
    Seq(below, above, inFront, behind, left, right).flatten

  val floodQueue: mutable.Queue[(Int, Int, Int)] = mutable.Queue()
  val flooded: mutable.Queue[(Int, Int, Int)] = mutable.Queue()
  floodQueue.appendAll(Seq((0, 0, 0)))
  while (floodQueue.nonEmpty) {
    val floodHere = floodQueue.removeHead()
    flooded.appendAll(Seq(floodHere))
    val nextFlood = floodable(floodHere)
      .filterNot(f => flooded.contains(f))
      .filterNot(f => floodQueue.contains(f))
    floodQueue.appendAll(nextFlood)
  }

  val inverse = Array.fill(dim, dim, dim)(true)
  toggleSpaces(inverse, flooded.toSeq, false)
  toggleSpaces(inverse, lavaCoords, false)

  val lavaSurfaceArea = surfaceArea(lavaSpace)
  val airSurfaceArea = surfaceArea(inverse)

  println(s"lava exposed sides: $lavaSurfaceArea\nair exposed sides: $airSurfaceArea\ndifference = ${lavaSurfaceArea - airSurfaceArea}")
}
