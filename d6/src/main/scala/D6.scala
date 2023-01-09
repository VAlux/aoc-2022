import scala.annotation.tailrec

@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val chars = input.head.toCharArray.toList
  val resultPart1 = Scanner.locateDuplicate(chars, 4)
  val resultPart2 = Scanner.locateDuplicate(chars, 14)
  println(s"Part 1: $resultPart1")
  println(s"Part 2: $resultPart2")

object Scanner:
  def locateDuplicate(input: List[Char], windowSize: Int): Int =
    def containsDuplicates(chunk: List[Char]): Boolean =
      chunk.toSet.size == chunk.size

    input.zipWithIndex.sliding(windowSize).find(chunk => containsDuplicates(chunk.map(_._1))) match
      case Some(chunk) => chunk.last._2 + 1
      case _           => -1
