@main def entrypoint: Unit =
  val input = FileLoader.readFile("input.txt")
  val elves = toElves(input)
  val calories = maxCalories(elves)
  val top3ElvesCaloriesSum = sumCalories(topElves(elves, 3)).sum
  println(top3ElvesCaloriesSum)

// represents elf carrying a bag of calories
type Elf = List[Int]

def toElves(
    input: List[String],
    current: List[String] = List.empty,
    result: List[Elf] = List.empty
): List[Elf] =
  if input.isEmpty then result
  else
    input.head match
      case c if c.nonEmpty => toElves(input.tail, current :+ c, result)
      case _               => toElves(input.tail, List.empty, result :+ current.map(_.toInt))

def sumCalories(elves: List[Elf]): Elf = elves.map(_.sum)

def maxCalories(elves: List[Elf]): Int = sumCalories(elves).max

def sortElves(elves: List[Elf]): List[Elf] = elves.sortBy(_.sum).reverse

def topElves(elves: List[Elf], n: Int): List[Elf] = sortElves(elves).take(n)
