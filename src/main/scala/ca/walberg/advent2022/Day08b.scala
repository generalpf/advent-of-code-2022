package ca.walberg.advent2022

import scala.io.Source

@main def Day08b(args: String*): Unit = {
  val source = Source.fromFile("data/day8.txt")
  val forest: Array[Array[Byte]] = source.getLines().foldLeft(Array[Array[Byte]]())(
    (acc: Array[Array[Byte]], line: String) =>
      acc :+ line.getBytes
  )
  var bestScore = 0
  for
    y <- forest.indices
    x <- forest(0).indices
  do
    val treeHeight = forest(y)(x)
    // from the west, then east, then north, then south
    val west = ((x - 1 until -1 by -1).span(x1 => forest(y)(x1) < treeHeight) match {
      case (head, tail) => head :++ tail.take(1)
    }).length
    val east = ((x + 1 until forest(0).length).span(x1 => forest(y)(x1) < treeHeight) match {
      case (head, tail) => head :++ tail.take(1)
    }).length
    val north = ((y - 1 until -1 by -1).span(y1 => forest(y1)(x) < treeHeight) match {
      case (head, tail) => head :++ tail.take(1)
    }).length
    val south = ((y + 1 until forest.length).span(y1 => forest(y1)(x) < treeHeight) match {
      case (head, tail) => head :++ tail.take(1)
    }).length
    bestScore = Math.max(bestScore, west * east * north * south)

  println(s"the best score is $bestScore")
  source.close()
}
