package ca.walberg.advent2022

import java.io.{BufferedReader, File, FileReader}
import scala.io.Source
@main def Day01b(args: String*): Unit = {
    val source = Source.fromFile("data/day1.txt")
    val caloriesPer: Seq[Int] = source.getLines().foldRight(Seq(0))(
      (line: String, acc: Seq[Int]) =>
        line match
          case "" => acc :+ 0
          case _ => acc.dropRight(1) :+ (acc.last + line.toInt)
    )
    println(caloriesPer.sortBy(-_).take(3).sum)
    source.close()
}