package ca.walberg.advent2022

import scala.io.Source

@main def Day25a(args: String*): Unit = {
  def snafuToLong(snafu: String): Long =
    snafu
      .reverse
      .zipWithIndex
      .foldLeft(0L)(
        (acc: Long, charAndIndex: (Char, Int)) =>
          val coefficient = charAndIndex._1 match
            case '=' => -2
            case '-' => -1
            case c => c - '0'
          acc + coefficient * Math.pow(5.0, charAndIndex._2).toLong
      )

  def longToSnafu(long: Long): String =
    var remainder = long
    var snafu = ""
    var power = (0 until 100).find(pow =>
      val foo = (remainder / Math.pow(5.0, pow)).ceil.toInt
      foo >= 0 && foo <= 2
    ).head
    while (power >= 0) {
      val base = Math.pow(5.0, power)
      var quotient = remainder.toDouble / base
      if (quotient > 2.5) {
        val prevDigit = snafu.last
        snafu = snafu.dropRight(1) + (prevDigit match
          case '0' => "1"
          case '1' => "2"
          )
        remainder -= (base * 5).toLong
        quotient = remainder / base
      } else if (quotient < -2.5) {
        val prevDigit = snafu.last
        snafu = snafu.dropRight(1) + (prevDigit match
          case '0' => '-'
          case '-' => '='
          )
        remainder += (base * 5).toLong
        quotient = remainder / base
      }
      val coefficient = quotient.toLong
      snafu += (coefficient match
        case 2 => '2'
        case 1 => '1'
        case 0 => '0'
        case -1 => '-'
        case -2 => '='
        )
      remainder -= (coefficient * base).toLong
      power -= 1
    }
    if (snafu.head == '0')
      snafu.drop(1)
    else
      snafu

  // mini test suite.
  Seq(
    ("1=-0-2", 1747),
    ("12111", 906),
    ("2=0=", 198),
    ("21", 11),
    ("2=01", 201),
    ("111", 31),
    ("20012", 1257),
    ("112", 32),
    ("1=-1=", 353),
    ("1-12", 107),
    ("12", 7),
    ("1=", 3),
    ("122", 37),
  ).foreach((snafu, int) =>
    assert(snafuToLong(snafu) == int.toLong)
    assert(longToSnafu(int.toLong) == snafu)
  )

  val source = Source.fromFile("data/day25.txt")
  val sum = source.getLines()
    .map(snafuToLong)
    .sum
  source.close()

  println(s"sum is $sum, SNAFU number is ${longToSnafu(sum)}")
}
