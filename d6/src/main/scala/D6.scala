import scala.annotation.tailrec

@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val result = Scanner.locateDuplicate(input.head.toCharArray.toList)
  println(result)

object Scanner:
  def locateDuplicate(input: List[Char]): Int =
    def containsDuplicates(chunk: List[Char]): Boolean =
      chunk.toSet.size == chunk.size

    input.zipWithIndex.sliding(14).find(chunk => containsDuplicates(chunk.map(_._1))) match
      case Some(chunk) => chunk.last._2 + 1
      case _           => -1
