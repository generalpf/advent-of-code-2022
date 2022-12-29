package ca.walberg.advent2022

import scala.io.Source

@main def Day23b(args: String*): Unit = {
  val source = Source.fromFile("data/day23.txt")
  var grove: Array[String] = source.getLines().toArray
  source.close()

  var rounds = 0
  var elvesMoved = Int.MaxValue
  while (elvesMoved > 0) {
    // 0 = north, 1 = south, 2 = west, 3 = east
    val considerFirst = rounds % 4
    // grow it to make the indexing easier.
    grove = Array(String(Array.fill(grove.head.length + 2)('.'))) ++ grove.map("." + _ + ".") ++ Array(String(Array.fill(grove.head.length + 2)('.')))

    val elves = grove.indices.flatMap(y =>
      grove(y).indices.filter(x =>
        grove(y)(x) == '#'
      ).map((_, y))
    )

    val proposed = elves.map((x, y) =>
      val n = elves.contains((x, y - 1))
      val s = elves.contains((x, y + 1))
      val w = elves.contains((x - 1, y))
      val e = elves.contains((x + 1, y))
      val nw = elves.contains((x - 1, y - 1))
      val ne = elves.contains((x + 1, y - 1))
      val sw = elves.contains((x - 1, y + 1))
      val se = elves.contains((x + 1, y + 1))
      if (!n && !s && !w && !e && !nw && !ne && !sw && !se)
        None
      else
        (0 until 4)
          .map(d => (d + considerFirst) % 4)
          .find(d =>
            d match {
              case 0 => !n && !nw && !ne
              case 1 => !s && !sw && !se
              case 2 => !w && !nw && !sw
              case 3 => !e && !ne && !se
            }
          )
          .map {
            case 0 => (x, y - 1)
            case 1 => (x, y + 1)
            case 2 => (x - 1, y)
            case 3 => (x + 1, y)
          }
    )

    elvesMoved = elves
      .zip(proposed)
      .count((coords, prop) =>
        // move this elf if they're the only one going to this location.
        prop match {
          case None =>
            false
          case Some(prop) =>
            if (proposed.flatten.count(e => e._1 == prop._1 && e._2 == prop._2) == 1) {
              grove(coords._2) = grove(coords._2).updated(coords._1, '.')
              grove(prop._2) = grove(prop._2).updated(prop._1, '#')
              true
            } else {
              false
            }
        }
      )

    rounds += 1
    println(s"${java.util.Date()} on round $rounds we moved $elvesMoved elves")
  }

  println(s"there were $rounds rounds")
}
