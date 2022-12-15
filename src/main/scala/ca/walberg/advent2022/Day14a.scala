package ca.walberg.advent2022

import scala.:+
import scala.io.Source

@main def Day14a(args: String*): Unit = {
  enum Space(val display: Char):
    case Empty extends Space('.')
    case Wall extends Space('#')
    case Sand extends Space('o')

  val source = Source.fromFile("data/day14.txt")
  val startRegex = """^(\d+),(\d+)""".r
  val nextRegex = """^ -> (\d+),(\d+)""".r
  val cave: Array[Array[Space]] = Array.fill(200, 600)(Space.Empty)
  source.getLines().foreach(line =>
    startRegex.findFirstMatchIn(line) match
      case Some(m) =>
        var fillerX = m.group(1).toInt
        var fillerY = m.group(2).toInt
        cave(fillerY)(fillerX) = Space.Wall
        var path = line.substring(m.group(0).length)
        while (path.nonEmpty) {
          nextRegex.findFirstMatchIn(path) match
            case Some(m) =>
              val targetX = m.group(1).toInt
              val targetY = m.group(2).toInt
              while (fillerX != targetX || fillerY != targetY) {
                if (fillerX < targetX) {
                  fillerX += 1
                } else if (fillerX > targetX) {
                  fillerX -= 1
                } else if (fillerY < targetY) {
                  fillerY += 1
                } else if (fillerY > targetY) {
                  fillerY -= 1
                }
                cave(fillerY)(fillerX) = Space.Wall
              }
              path = path.substring(m.group(0).length)
        }
  )

  var grains = 0
  try {
    while (true) {
      var atRest = false
      var sandX = 500
      var sandY = 0
      while (!atRest) {
        (cave(sandY + 1)(sandX), cave(sandY + 1)(sandX - 1), cave(sandY + 1)(sandX + 1)) match
          case (Space.Empty, _, _) =>
            sandY += 1
          case (_, Space.Empty, _) =>
            sandY += 1
            sandX -= 1
          case (_, _, Space.Empty) =>
            sandY += 1
            sandX += 1
          case _ =>
            atRest = true
      }
      cave(sandY)(sandX) = Space.Sand
      grains += 1
    }
  } catch {
    case _: ArrayIndexOutOfBoundsException =>
  }

  println(s"$grains grains fell")
  cave.indices.foreach(y =>
    (450 until 600).foreach(x =>
      print(cave(y)(x).display)
    )
    println
  )
  source.close()
}
