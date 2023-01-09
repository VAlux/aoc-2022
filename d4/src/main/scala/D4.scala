@main def entrypoint() =
  import RangeExplorer.*
  val input = FileLoader.readFile("input.txt")
  val ranges = input.map(RangeParser.parseRangePair).flatten
  val inclusionsAmount = ranges.count(RangeExplorer.isInclusivePair)
  println(inclusionsAmount)

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
  def isInclusivePair(pair: RangePair): Boolean =
    (pair.right contains pair.left) || (pair.left contains pair.right)

  extension (left: Range)
    def contains(right: Range): Boolean =
      left.from <= right.from && left.to >= right.to
