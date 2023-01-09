@main def entrypoint() =
  import RangeExplorer.*
  val input = FileLoader.readFile("input.txt")
  val ranges = input.map(RangeParser.parseRangePair).flatten
  val inclusionsAmount = ranges.count(RangeExplorer.isInclusivePair)
  val overlapsAmount = ranges.count(RangeExplorer.isOverlappingPair)
  println(inclusionsAmount)
  println(overlapsAmount)

case class Range(from: Int, to: Int)
case class RangePair(left: Range, right: Range)

object RangeParser:
  def parseRange(input: String): Option[Range] =
    input.split('-') match
      case Array(a, b) =>
        for
          left <- a.toIntOption
          right <- b.toIntOption
        yield Range(left, right)
      case _ => None

  def parseRangePair(input: String): Option[RangePair] =
    input.split(',') match
      case Array(a, b) =>
        for
          left <- parseRange(a)
          right <- parseRange(b)
        yield RangePair(left, right)
      case _ => None

object RangeExplorer:
  given Ordering[Range] with
    def compare(x: Range, y: Range): Int =
      x.from compare y.from

  def isInclusivePair(pair: RangePair): Boolean =
    (pair.left contains pair.right) || (pair.right contains pair.left)

  def isOverlappingPair(pair: RangePair): Boolean =
    List(pair.left, pair.right).sorted match
      case a :: b :: Nil => a overlaps b
      case _             => false

  extension (left: Range)
    def contains(right: Range): Boolean =
      left.from <= right.from && left.to >= right.to

    def overlaps(right: Range): Boolean =
      left.to >= right.from
