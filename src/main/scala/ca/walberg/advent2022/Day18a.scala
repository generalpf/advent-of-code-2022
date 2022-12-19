package ca.walberg.advent2022

import scala.io.Source

@main def Day18a(args: String*): Unit = {
  val source = Source.fromFile("data/day18.txt")
  val dim = 32
  val space: Array[Array[Array[Boolean]]] = source.getLines().foldLeft(Array.fill(dim, dim, dim)(false))(
    (acc: Array[Array[Array[Boolean]]], line: String) =>
      val coords = line.split(',').map(_.toInt + 1) // +1 so nothing is on the edge @ 0
      acc(coords(2))(coords(1))(coords(0)) = true
      acc
  )
  source.close()

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

  println(s"bottom up: $bottomUp\ntop down: $topDown\nleft to right: $fromLeft\nright to left: $fromRight\nfront to back: $fromFront\nback to front: $fromBack")
  println(s"total exposed sides: ${bottomUp + topDown + fromLeft + fromRight + fromFront + fromBack}")
}
