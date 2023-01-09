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
    def calculateOutcome(left: Shape, right: Shape): RoundOutcome =
      if left.counterpart == right then RoundOutcome.Lose
      else if right.counterpart == left then RoundOutcome.Win
      else RoundOutcome.Draw

    shapes.map((a, b) => Round(b, calculateOutcome(a, b)))

sealed trait Shape(val points: Int, val counterpart: Shape)
object Shape:
  case object A extends Shape(1, Z)
  case object B extends Shape(2, X)
  case object C extends Shape(3, Y)
  case object X extends Shape(1, C)
  case object Y extends Shape(2, A)
  case object Z extends Shape(3, B)

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
