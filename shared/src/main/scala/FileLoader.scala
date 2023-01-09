import scala.io.Source

object FileLoader:

  def readFile(fileName: String): List[String] =
    Source.fromResource(fileName, getClass.getClassLoader).getLines.toList
