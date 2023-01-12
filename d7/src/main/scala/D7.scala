import scala.annotation.tailrec

@main def entrypoint() =
  val input = FileLoader.readFile("input_test.txt")
  PromptReader.read(input).foreach(println)

// Recursive ADT, which represents the filesystem tree structure
sealed trait FSEntry(val size: Int)
object FSEntry:
  case object Directory extends FSEntry(0)
  case class File(bytes: Int) extends FSEntry(bytes)

// All the possible commands for working with filesystem, which can be interpreted
enum Program(val alias: String):
  case ChangeDirectory extends Program("cd")
  case ListStructure extends Program("ls")

object PromptReader:
  case class Prompt(val command: Program, val arguments: List[String] = List.empty, output: List[String] = List.empty)

  def read(input: List[String]): List[Prompt] =
    input.foldLeft(List.empty[Prompt]) { case (acc, current) =>
      current.split(" ").toList match
        case "$" :: prompt => readProgram(prompt).map(program => acc :+ program).getOrElse(acc)
        case output        => acc.updated(acc.size - 1, acc.last.copy(output = acc.last.output ::: output))
    }

  def readProgram(input: List[String]): Option[Prompt] =
    Program.values.find(_.alias == input.head).map(program => Prompt(program, input.tail))
