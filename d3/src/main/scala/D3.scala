import scala.util.chaining.*

@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val rucksacks = RucksackParser.parse(input)
  val commonItemsPrioritiesSum = ItemPriority.sumPriorities(rucksacks.flatMap(RucksackScanner.findCommonItem))
  val commonItemInGroupsOf3Elves =
    rucksacks.grouped(3).flatMap(RucksackScanner.findCommonItems).toList.pipe(ItemPriority.sumPriorities)

  println(commonItemsPrioritiesSum)
  println(commonItemInGroupsOf3Elves)

case class Rucksack(val items: List[Char]):
  val firstCompartment = items.slice(0, items.length / 2).toSet
  val secondCompartment = items.slice(items.length / 2, items.length).toSet

object RucksackParser:
  def parse(input: String): Rucksack = Rucksack(input.toCharArray.toList)
  def parse(input: List[String]): List[Rucksack] = input.map(parse)

object RucksackScanner:
  private def findCommonItem(first: Set[Char], second: Set[Char]): Option[Char] =
    first.intersect(second).headOption

  def findCommonItem(rucksack: Rucksack): Option[Char] =
    findCommonItem(rucksack.firstCompartment, rucksack.secondCompartment)

  def findCommonItems(rucksacks: List[Rucksack]): Set[Char] =
    rucksacks.map(_.items.toSet).reduce(_ intersect _)

object ItemPriority:
  private val priorities: Map[Char, Int] =
    (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map((char, index) => (char, index + 1)).toMap

  def sumPriorities(items: List[Char]): Int =
    items.flatMap(priorities.get).sum
