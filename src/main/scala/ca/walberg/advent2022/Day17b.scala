package ca.walberg.advent2022

import scala.io.Source

@main def Day17b(args: String*): Unit = {
  val source = Source.fromFile("data/day17.txt")
  val vents: String = source.getLines().next()
  source.close()

  // packed rock patterns, bottom to top.
  val rockPatterns: Seq[Seq[Seq[Boolean]]] = Seq(
    // line
    Seq(
      Seq(true, true, true, true),
    ),
    // plus
    Seq(
      Seq(false, true, false),
      Seq(true, true, true),
      Seq(false, true, false),
    ),
    // L
    Seq(
      Seq(true, true, true),
      Seq(false, false, true),
      Seq(false, false, true),
    ),
    // |
    Seq(
      Seq(true),
      Seq(true),
      Seq(true),
      Seq(true),
    ),
    // cube
    Seq(
      Seq(true, true),
      Seq(true, true),
    ),
  )

  // in reverse order (last element is the top row)
  var chamber: Array[Array[Boolean]] = Array.fill(1, 7)(true)
  var rocks = 0L
  var step = 0L
  var ventPosAtCheckpoint = -1
  var rockPatternAtCheckpoint = -1
  var heightAtCheckpoint = -1L
  var rockCountAtCheckpoint = -1L
  var heightFromCycles = 0L
  while (rocks < 1000000000000L) {
    // ayyy which rock we droppin'?
    val rockPattern = rockPatterns((rocks % rockPatterns.length).toInt)

    val topOfStack = chamber.lastIndexWhere(_.contains(true))
    // cycle detection!
    var rockX = 2
    var rockY = topOfStack + 4

    // lengthen the chamber to fit the piece
    (0 until (rockY + rockPattern.length - chamber.length)).foreach(_ => chamber = chamber :+ Array.fill(7)(false))

    var stopped = false
    while (!stopped) {
      vents((step % vents.length).toInt) match
        case '<' =>
          if (rockX > 0 && !rockPattern.indices.exists(y =>
            rockPattern(y).indices.exists(x =>
              rockPattern(y)(x) && chamber(rockY + y)(rockX + x - 1)
            )
          ))
            rockX -= 1
        case '>' =>
          if (rockX + rockPattern(0).length < chamber(0).length && !rockPattern.indices.exists(y =>
            rockPattern(y).indices.exists(x =>
              rockPattern(y)(x) && chamber(rockY + y)(rockX + x + 1)
            )
          ))
            rockX += 1

      // can it drop?
      stopped = rockPattern.indices.exists(y =>
        rockPattern(y).indices.exists(x =>
          rockPattern(y)(x) && chamber(rockY + y - 1)(rockX + x)
        )
      )
      if (!stopped)
        rockY -= 1
      else
        // the rock BECOMES the chamber.
        rockPattern.indices.foreach(y =>
          rockPattern(y).indices.foreach(x =>
            if (rockPattern(y)(x))
              chamber(rockY + y)(rockX + x) = true
          )
        )

      step += 1
    }

    if (rocks == 1000L) {
      rockPatternAtCheckpoint = (rocks % rockPatterns.length).toInt
      ventPosAtCheckpoint = (step % vents.length).toInt
      heightAtCheckpoint = chamber.lastIndexWhere(_.contains(true))
      rockCountAtCheckpoint = rocks
      println(s"after 1,000 rocks, we are at rock pattern $rockPatternAtCheckpoint and vent position $ventPosAtCheckpoint")
    } else if (rockPatternAtCheckpoint >= 0 && (rocks % rockPatterns.length).toInt == rockPatternAtCheckpoint && (step % vents.length).toInt == ventPosAtCheckpoint) {
      val topOfStack = chamber.lastIndexWhere(_.contains(true))
      val rocksPerCycle = rocks - rockCountAtCheckpoint
      val heightPerCycle = topOfStack - heightAtCheckpoint
      println(s"cycle detected! after $rocksPerCycle rocks, we grew by $heightPerCycle. fast-forwarding!")
      while (rocks < 1000000000000L - rocksPerCycle) {
        rocks += rocksPerCycle
        heightFromCycles += heightPerCycle
      }
      heightAtCheckpoint = -1
      ventPosAtCheckpoint = -1
      rockCountAtCheckpoint = -1
    }

    rocks += 1
  }

  println(chamber.lastIndexWhere(_.contains(true)) + heightFromCycles)
}
