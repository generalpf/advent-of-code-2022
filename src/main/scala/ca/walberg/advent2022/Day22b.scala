package ca.walberg.advent2022

import scala.io.Source

@main def Day22b(args: String*): Unit = {
  val source = Source.fromFile("data/day22.txt")
  val lines = source.getLines().toArray
  source.close()

  val board: Seq[String] = lines.dropRight(2)
  var instructions: String = lines.last

  val faceWidth = board.head.length / 3
  val faceHeight = board.length / 4

  val faces = Seq(
    board.take(faceHeight).map(_.substring(faceWidth, faceWidth * 2)),
    board.take(faceHeight).map(_.takeRight(faceWidth)),
    board.slice(faceHeight, faceHeight * 2).map(_.substring(faceWidth, faceWidth * 2)),
    board.slice(faceHeight * 2, faceHeight * 3).map(_.take(faceWidth)),
    board.slice(faceHeight * 2, faceHeight * 3).map(_.substring(faceWidth, faceWidth * 2)),
    board.takeRight(faceHeight).map(_.take(faceWidth))
  )

  faces.foreach(face =>
    assert(face.length == faceHeight)
    assert(face.head.length == faceWidth)
    assert(-1 == face.indexWhere(_.indexOf(' ') != -1))
  )

  // x (from 0), y (from 0), facing (right = 0, down = 1, left = 2, up = 3)
  var x = faces.head.head.indexOf('.')
  var y = 0
  var facing = 0
  var face = 0

  while (instructions.nonEmpty) {
    instructions.head match
      case 'L' =>
        facing = (facing + 3) % 4
        instructions = instructions.drop(1)
      case 'R' =>
        facing = (facing + 1) % 4
        instructions = instructions.drop(1)
      case c if c.isDigit =>
        val regex = "^(\\d+).*$".r
        regex.findFirstMatchIn(instructions) match
          case Some(m) =>
            val distance = m.group(1).toInt
            instructions = instructions.drop(m.group(1).length)
            (0 until distance).foreach(_ =>
              val (nextX, nextY, nextFace, nextFacing) = facing match {
                case 0 =>
                  // going right
                  if (x == faceWidth - 1) {
                    face match {
                      case 0 =>
                        // to our right is face 1 (same orientation)
                        (0, y, 1, facing)
                      case 1 =>
                        // to our right is face 4 (upside-down)
                        (faceWidth - 1, faceHeight - y - 1, 4, 2)
                      case 2 =>
                        // to our right is face 1 (rotated 90 degrees clockwise)
                        (y, faceHeight - 1, 1, 3)
                      case 3 =>
                        // to our right is face 4 (same orientation)
                        (0, y, 4, facing)
                      case 4 =>
                        // to our right is face 1 (upside-down)
                        (faceWidth - 1, faceHeight - y - 1, 1, 2)
                      case 5 =>
                        // to our right is face 4 (rotated 90 degrees clockwise)
                        (y, faceHeight - 1, 4, 3)
                    }
                  } else {
                    (x + 1, y, face, facing)
                  }
                case 2 =>
                  // going left
                  if (x == 0) {
                    face match {
                      case 0 =>
                        // to our left is face 3 (upside-down)
                        (0, faceHeight - y - 1, 3, 0)
                      case 1 =>
                        // to our left is face 0 (same orientation)
                        (faceWidth - 1, y, 0, facing)
                      case 2 =>
                        // to our left is face 3 (rotated 90 degrees clockwise)
                        (y, 0, 3, 1)
                      case 3 =>
                        // to our left is face 0 (upside-down)
                        (0, faceHeight - y - 1, 0, 0)
                      case 4 =>
                        // to our left is face 3 (same orientation)
                        (faceWidth - 1, y, 3, facing)
                      case 5 =>
                        // to our left is face 0 (rotated 90 degrees clockwise)
                        (y, 0, 0, 1)
                    }
                  } else {
                    (x - 1, y, face, facing)
                  }
                case 1 =>
                  // going down
                  if (y == faceHeight - 1) {
                    face match {
                      case 0 =>
                        // below us is face 2 (same orientation)
                        (x, 0, 2, facing)
                      case 1 =>
                        // below us is face 2 (rotated 90 degrees counterclockwise)
                        (faceWidth - 1, x, 2, 2)
                      case 2 =>
                        // below us is face 4 (same orientation)
                        (x, 0, 4, facing)
                      case 3 =>
                        // below us is face 5 (same orientation)
                        (x, 0, 5, facing)
                      case 4 =>
                        // below us is face 5 (rotated 90 degrees counterclockwise)
                        (faceWidth - 1, x, 5, 2)
                      case 5 =>
                        // below us is face 1 (same orientation)
                        (x, 0, 1, facing)
                    }
                  } else {
                    (x, y + 1, face, facing)
                  }
                case 3 =>
                  // going up
                  if (y == 0) {
                    face match {
                      case 0 =>
                        // above us is face 5 (rotated 90 degrees counterclockwise)
                        (0, x, 5, 0)
                      case 1 =>
                        // above us is face 5 (same orientation)
                        (x, faceHeight - 1, 5, facing)
                      case 2 =>
                        // above us is face 0 (same orientation)
                        (x, faceHeight - 1, 0, facing)
                      case 3 =>
                        // above us is face 2 (rotated 90 degrees counterclockwise)
                        (0, x, 2, 0)
                      case 4 =>
                        // above us is face 2 (same orientation)
                        (x, faceHeight - 1, 2, facing)
                      case 5 =>
                        // above us is face 3 (same orientation)
                        (x, faceHeight - 1, 3, facing)
                    }
                  } else {
                    (x, y - 1, face, facing)
                  }
              }

              if (faces(nextFace)(nextY)(nextX) == '.') {
                y = nextY
                x = nextX
                face = nextFace
                facing = nextFacing
              }
            )
  }

  val finalX = x + faceWidth * (face match
    case 0 => 1
    case 1 => 2
    case 2 => 1
    case 3 => 0
    case 4 => 1
    case 5 => 0
    ) + 1
  val finalY = y + faceHeight * (face match
    case 0 => 0
    case 1 => 0
    case 2 => 1
    case 3 => 2
    case 4 => 2
    case 5 => 3
    ) + 1

  val password = 1000 * finalY + 4 * finalX + facing
  println(s"finally at ($x, $y) on face $face, facing $facing, password could be $password")
}