import scala.annotation.tailrec

@main def entrypoint() =
  val input = FileLoader.readFile("input_test.txt")
  val prompt = PromptReader.read(input)
  prompt.foreach(println)
  val fs = FSReader.read(prompt)
  println(fs)

// Recursive ADT, which represents the filesystem tree structure
sealed trait FSEntry(val name: String)
object FSEntry:
  case class File(override val name: String, val size: Int) extends FSEntry(name)
  case class Directory(override val name: String, val content: List[FSEntry]) extends FSEntry(name)

// All the possible commands for working with filesystem, which can be interpreted
enum Program(val alias: String):
  case ChangeDirectory extends Program("cd")
  case ListStructure extends Program("ls")

object PromptReader:
  case class Prompt(
      val command: Program,
      val arguments: List[String] = List.empty,
      val output: List[List[String]] = List.empty
  )

  def read(input: List[String]): List[Prompt] =
    input.foldLeft(List.empty[Prompt]) { case (acc, current) =>
      current.split(" ").toList match
        case "$" :: prompt => readProgramPrompt(prompt).map(program => acc :+ program).getOrElse(acc)
        case output        => acc.updated(acc.size - 1, acc.last.copy(output = acc.last.output :+ output))
    }

  private def readProgramPrompt(input: List[String]): Option[Prompt] =
    Program.values.find(_.alias == input.head).map(program => Prompt(program, input.tail))

object FSReader:
  import PromptReader.Prompt
  import Program.*
  import FSEntry.*

  def read(prompt: List[Prompt]): FSEntry =
    def go(current: Prompt, rem: List[Prompt], dir: Directory): Directory =
      println(current)
      if rem.isEmpty then dir
      else
        current.command match
          case ListStructure =>
            val content = current.output.flatMap(_.map(_.split(" ").toList)).map {
              case "dir" :: name    => Directory(name.head, List.empty)
              case size :: filename => File(filename.head, size.toInt)
              case Nil              => throw RuntimeException(s"unknown prompt: [${current.command}]")
            }

            go(rem.head, rem.tail, dir.copy(content = dir.content ::: content))
          case ChangeDirectory =>
            dir.content.find(a => a.name == current.arguments.head) match
              case None        => throw new RuntimeException(s"unknown directory: [${current.arguments.head}]")
              case Some(value) => go(rem.head, rem.tail, value.asInstanceOf[Directory])

    go(prompt.head, prompt.tail, Directory("root", List(Directory("/", List.empty))))
