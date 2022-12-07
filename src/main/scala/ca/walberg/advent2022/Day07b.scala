package ca.walberg.advent2022

import scala.io.Source

@main def Day07b(args: String*): Unit = {

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

  def traverse(directory: Directory): Unit =
    directory.subdirectories.foreach(traverse)
    directory.size = Some(directory.files.map(_.size).sum + directory.subdirectories.map(_.size.head).sum)

  traverse(rootDirectory)

  val spaceNeeded = -(70000000L - 30000000L - rootDirectory.size.head)

  def findBigEnoughDirectories(directory: Directory): Seq[Directory] =
    directory.subdirectories.filter(_.size.head >= spaceNeeded) ++
      directory.subdirectories.flatMap(findBigEnoughDirectories)

  val recoveredSpace = findBigEnoughDirectories(rootDirectory).map(_.size.head).min

  println(s"space needed: $spaceNeeded, can recover $recoveredSpace")
  source.close()
}
