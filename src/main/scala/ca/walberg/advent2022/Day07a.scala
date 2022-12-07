package ca.walberg.advent2022

import scala.io.Source

@main def Day07a(args: String*): Unit = {

  class File(var name: String, var size: Long)

  class Directory(var name: String, var files: Seq[File], var subdirectories: Seq[Directory], var parent: Option[Directory], var size: Option[Long])

  val rootDirectory = Directory("/", Seq(), Seq(), None, None)
  var currentDirectory: Directory = rootDirectory

  val source = Source.fromFile("data/day7.txt")
  source.getLines().foreach(
    line =>
      line.head match
        case '$' =>
          line.substring(2, 4) match
            case "cd" =>
              val target = line.substring(5)
              target match
                case "/" => currentDirectory = rootDirectory
                case ".." => currentDirectory = currentDirectory.parent.getOrElse(rootDirectory)
                case _ => currentDirectory = currentDirectory.subdirectories.find(s => s.name == target).head
            case _ =>
        case 'd' =>
          val dirName = line.substring(4)
          currentDirectory.subdirectories :+= Directory(dirName, Seq(), Seq(), Some(currentDirectory), None)
        case _ =>
          val parts = line.split(' ')
          currentDirectory.files :+= File(parts.last, parts.head.toLong)
  )

  var matchingDirectories = Seq[Directory]()
  def traverse(directory: Directory): Unit =
    directory.subdirectories.foreach(d =>
      traverse(d)
      if (d.size.head <= 100000L)
        matchingDirectories +:= d
    )
    directory.size = Some(directory.files.map(_.size).sum + directory.subdirectories.map(_.size.head).sum)

  traverse(rootDirectory)

  val totalSize = matchingDirectories.map(_.size.head).sum
  println(s"total size of small directories: $totalSize")
  source.close()
}
