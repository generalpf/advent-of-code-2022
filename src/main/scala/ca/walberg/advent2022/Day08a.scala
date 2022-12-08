package ca.walberg.advent2022

import scala.io.Source

@main def Day08a(args: String*): Unit = {
  val source = Source.fromFile("data/day8.txt")
  val forest: Array[Array[Byte]] = source.getLines().foldLeft(Array[Array[Byte]]())(
    (acc: Array[Array[Byte]], line: String) =>
      acc :+ line.getBytes
  )
  var visibleTrees = 0
  for
    y <- forest.indices
    x <- forest(0).indices
  do
    val treeHeight = forest(y)(x)
    // from the west, then east, then north, then south
    if ((0 until x).forall(x1 => forest(y)(x1) < treeHeight)
        || (x + 1 until forest(0).length).forall(x1 => forest(y)(x1) < treeHeight)
        || (0 until y).forall(y1 => forest(y1)(x) < treeHeight)
        || (y + 1 until forest.length).forall(y1 => forest(y1)(x) < treeHeight))
      visibleTrees += 1

  println(s"there are $visibleTrees visible trees")
  source.close()
}
