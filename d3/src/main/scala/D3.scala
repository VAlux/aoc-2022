@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val rucksacks = RucksackParser.parse(input)
  val commonItemsPrioritiesSum = ItemPriority.sumPriorities(rucksacks.flatMap(findCommonItem))
  println(commonItemsPrioritiesSum)

case class Rucksack(val items: List[Char]):
  val firstCompartment = items.slice(0, items.length / 2).toSet
  val secondCompartment = items.slice(items.length / 2, items.length).toSet

object RucksackParser:
  def parse(input: String): Rucksack = Rucksack(input.toCharArray.toList)
  def parse(input: List[String]): List[Rucksack] = input.map(parse)

def findCommonItem(rucksack: Rucksack): Option[Char] =
  rucksack.firstCompartment.intersect(rucksack.secondCompartment).headOption

object ItemPriority:
  def sumPriorities(items: List[Char]): Int =
    items.flatMap(priorities.get).sum

  private val priorities: Map[Char, Int] =
    (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map((char, index) => (char, index + 1)).toMap
