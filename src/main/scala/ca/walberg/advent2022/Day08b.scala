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
    // this is ugly, I'd like to use takeUntil() but Scala doesn't have it.
    var blockingTrees = 0
    val west = (x - 1 until -1 by -1).takeWhile(x1 =>
      if (forest(y)(x1) >= treeHeight)
        blockingTrees += 1
      forest(y)(x1) < treeHeight
    ).length + blockingTrees
    blockingTrees = 0
    val east = (x + 1 until forest(0).length).takeWhile(x1 =>
      if (forest(y)(x1) >= treeHeight)
        blockingTrees += 1
      forest(y)(x1) < treeHeight
    ).length + blockingTrees
    blockingTrees = 0
    val north = (y - 1 until -1 by -1).takeWhile(y1 =>
      if (forest(y1)(x) >= treeHeight)
        blockingTrees += 1
      forest(y1)(x) < treeHeight
    ).length + blockingTrees
    blockingTrees = 0
    val south = (y + 1 until forest.length).takeWhile(y1 =>
      if (forest(y1)(x) >= treeHeight)
        blockingTrees += 1
      forest(y1)(x) < treeHeight
    ).length + blockingTrees
    val visibleTrees = west * east * north * south
    bestScore = Math.max(bestScore, visibleTrees)

  println(s"the best score is $bestScore")
  source.close()
}
