package ca.walberg.advent2022

import scala.io.Source

@main def Day22a(args: String*): Unit = {
  val source = Source.fromFile("data/day22.txt")
  val lines = source.getLines().toArray
  source.close()

  val board: Seq[String] = lines.dropRight(2)
  var instructions: String = lines.last

  // x (from 0), y (from 0), facing (right = 0, down = 1, left = 2, up = 3)
  var x = lines.head.indexOf('.')
  var y = 0
  var facing = 0

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
              val (nextX, nextY) = facing match
                case 3 =>
                  // look up
                  val above = y - 1
                  if (above == -1 || board(above).length < x || board(above)(x) == ' ') {
                    val wrap = board.lastIndexWhere(row => row.length >= x && row(x) != ' ')
                    (x, wrap)
                  } else {
                    (x, above)
                  }
                case 1 =>
                  // look down
                  val below = y + 1
                  if (below >= board.length || board(below).length < x || board(below)(x) == ' ') {
                    val wrap = board.indexWhere(row => row.length >= x && row(x) != ' ')
                    (x, wrap)
                  } else {
                    (x, below)
                  }
                case 0 =>
                  // look right
                  val right = x + 1
                  if (right >= board(y).length || board(y)(right) == ' ') {
                    val wrap = board(y).indexWhere(c => c != ' ')
                    (wrap, y)
                  } else {
                    (right, y)
                  }
                case 2 =>
                  // look left
                  val left = x - 1
                  if (left == -1 || board(y)(left) == ' ') {
                    val wrap = board(y).lastIndexWhere(c => c != ' ')
                    (wrap, y)
                  } else {
                    (left, y)
                  }
              if (board(nextY)(nextX) == '.') {
                y = nextY
                x = nextX
              }
            )
  }

  val password = 1000 * (y + 1) + 4 * (x + 1) + facing
  println(s"finally at ($x, $y), password is $password")
}
