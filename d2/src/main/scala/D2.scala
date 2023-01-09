import Shape.A
import Shape.B
import Shape.C
import Shape.X
import Shape.Y
import Shape.Z
@main def entrypoint() =
  val input = FileLoader.readFile("input.txt")
  val shapes = ShapeParser.parseShapes(input).flatten
  val rounds = Round.calculateRounds(shapes)
  val total = rounds.map(_.score).sum
  println(total)

object Round:
  enum RoundOutcome(val points: Int):
    case Lose extends RoundOutcome(0)
    case Draw extends RoundOutcome(3)
    case Win extends RoundOutcome(6)

  case class Round(val shape: Shape, val outcome: RoundOutcome):
    val score = shape.points + outcome.points

  def calculateRounds(shapes: List[(Shape, Shape)]): List[Round] =
    extension (left: Shape)
      def vs(right: Shape): RoundOutcome =
        if left.counterpart == right then RoundOutcome.Lose
        else if right.counterpart == left then RoundOutcome.Win
        else RoundOutcome.Draw

    def strategy(left: Shape, right: Shape): Round =
      val targetOutcome = right match
        case A => RoundOutcome.Lose
        case B => RoundOutcome.Draw
        case C => RoundOutcome.Win
        case X => RoundOutcome.Lose
        case Y => RoundOutcome.Draw
        case Z => RoundOutcome.Win

      val targetShape = targetOutcome match
        case RoundOutcome.Lose => left.counterpart
        case RoundOutcome.Draw => left
        case RoundOutcome.Win  => left.weak

      Round(targetShape, targetOutcome)

    shapes.map((a, b) => strategy(a, b))

sealed trait Shape(val points: Int, val counterpart: Shape, val weak: Shape)
object Shape:
  case object A extends Shape(1, Z, Y)
  case object B extends Shape(2, X, Z)
  case object C extends Shape(3, Y, X)
  case object X extends Shape(1, C, B)
  case object Y extends Shape(2, A, C)
  case object Z extends Shape(3, B, A)

object ShapeParser:
  private val shapeDescriptionMapping =
    Map(
      "A" -> Shape.A,
      "B" -> Shape.B,
      "C" -> Shape.C,
      "X" -> Shape.X,
      "Y" -> Shape.Y,
      "Z" -> Shape.Z
    )

  def parseShapes(input: List[String]): List[Option[(Shape, Shape)]] =
    input.map { round =>
      round.split(" ") match
        case Array(a: String, b: String) =>
          for
            shapeA <- parseShape(a)
            shapeB <- parseShape(b)
          yield (shapeA, shapeB)
        case _ => None
    }

  private def parseShape(shape: String): Option[Shape] =
    shapeDescriptionMapping.get(shape)
