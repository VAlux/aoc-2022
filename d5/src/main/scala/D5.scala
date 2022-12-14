import scala.annotation.tailrec

@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val stackDefinitions = input.takeWhile(_.nonEmpty)
  val moveDefinitions = input.dropWhile(_.nonEmpty).drop(1)
  val stacks = StackParser.parse(stackDefinitions)
  val moves = MoveParser.parse(moveDefinitions)
  val result = Crane(false).run(stacks, moves)
  println(result.map(_.head).mkString)

type CrateStack = List[Char]
case class Move(val amount: Int, val from: Int, val to: Int)

class Crane(val canMoveMultiple: Boolean = true):
  private def take(stacks: List[CrateStack], move: Move): (CrateStack, List[CrateStack]) =
    val stack = stacks(move.from)
    val taken = stack.take(move.amount)
    val currentState = stacks.updated(move.from, stack.drop(move.amount))
    (taken, currentState)

  private def put(target: CrateStack, stacks: List[CrateStack], move: Move): List[CrateStack] =
    stacks.updated(move.to, (if canMoveMultiple then target else target.reverse) ++ stacks(move.to))

  private def step(stacks: List[CrateStack], move: Move): List[CrateStack] =
    val (taken, current) = take(stacks, move)
    put(taken, current, move)

  @tailrec
  final def run(stacks: List[CrateStack], moves: List[Move]): List[CrateStack] =
    if moves.isEmpty then stacks
    else run(step(stacks, moves.head), moves.tail)

object StackParser:
  def parse(input: List[String]): List[CrateStack] =
    def transpose(input: List[String]): List[List[String]] =
      input.filter(_.nonEmpty).map(_.toCharArray) match
        case Nil => Nil
        case xs  => xs.map(_.take(3).mkString) :: transpose(xs.map(_.drop(3).mkString))

    // replace empty spaces with '[-]' and then remove all spaces between crates ('] [' will become '][')
    val sanitized =
      input.map(_.replaceAllLiterally("    ", "[-]")).map(_.replaceAllLiterally(" ", ""))

    //transpose crates, drop all ']' and '[' + get rid of empty places
    transpose(sanitized.init)
      .map(_.map(_.replaceAllLiterally("[", "")).map(_.replaceAllLiterally("]", "")))
      .map(_.dropWhile(_ == "-"))
      .map(_.map(_.head))

object MoveParser:
  def parse(input: List[String]): List[Move] =
    input.flatMap { moveDefinition =>
      moveDefinition.split(" ") match
        case Array("move", amount, "from", from, "to", to) => Some(Move(amount.toInt, from.toInt - 1, to.toInt - 1))
        case _                                             => None
    }
