import scala.annotation.tailrec
import FSEntry.Directory
import FSEntry.File

@main def entrypoint() =
  val input = FileLoader.readFile("input_test.txt")
  val prompt = PromptReader.read(input)
  val fs = FSReader.read(prompt)
  println(fsWalk(fs.asInstanceOf[Directory].content.head.asInstanceOf[Directory]))
  println(fs)

// Recursive ADT, which represents the filesystem tree structure
sealed trait FSEntry(val name: String)
object FSEntry:
  case class File(override val name: String, val size: Int) extends FSEntry(name)
  case class Directory(override val name: String, content: List[FSEntry]) extends FSEntry(name)

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

def fsWalk(root: FSEntry.Directory, level: Int = 0): String =
  def indent(size: Int): String = "  " * size

  s"${indent(level)} - ${root.name} (dir) \n" + root.content.map {
    case dir: Directory   => fsWalk(dir, level + 1)
    case File(name, size) => s"${indent(level + 1)} - $name (file, size = $size)"
  }.mkString("\n")

object FSReader:
  import PromptReader.Prompt
  import Program.*
  import FSEntry.*

  def read(prompt: List[Prompt]): FSEntry =
    def go(currentPrompt: Option[Prompt], rem: List[Prompt], dir: Directory): Directory =
      currentPrompt match
        case None =>
          println(s"DONE! $currentPrompt")
          dir
        case Some(current) =>
          current.command match
            case ListStructure =>
              val content = current.output.map {
                case "dir" :: name    => Directory(name.head, List.empty)
                case size :: filename => File(filename.head, size.toInt)
                case Nil              => throw RuntimeException(s"unknown prompt: [${current.command}]")
              }

              val newDir = dir.copy(content = dir.content ::: content)
              if rem.isEmpty then newDir else go(rem.headOption, rem.tail, newDir)
            case ChangeDirectory =>
              if current.arguments.head == ".." then dir
              else
                dir.content.find(_.name == current.arguments.head) match
                  case None => throw new RuntimeException(s"unknown directory: [${current.arguments.head}]")
                  case Some(value: Directory) =>
                    dir.copy(content =
                      dir.content.updated(
                        dir.content.indexOf(value),
                        go(rem.headOption, rem.tail, value)
                      )
                    )
                  case _ => throw RuntimeException(s"unknown prompt: [${current.command}]")

    val root = Directory("/", List.empty)
    go(prompt.headOption, prompt.tail, Directory("root", List(root)))
