package day03

import scala.io.Source

val source = Source.fromResource("day03.in")
val input: Matrix = source.getLines().map(_.toIndexedSeq).toIndexedSeq

type Matrix = IndexedSeq[IndexedSeq[Char]]
case class Coordinates(x: Int, y: Int)
case class NumberSlice(y: Int, xLower: Int, xUpper: Int)

def surrounding(x: Int, y: Int)(using matrix: Matrix): Set[Coordinates] = {
    val height = matrix.length
    val width = matrix(0).length
    def inRange(i: Int, upperEx: Int): Boolean = 0 <= i && i < upperEx
    Set(
        Coordinates(x-1, y-1), Coordinates(x  , y-1), Coordinates(x+1, y-1),
        Coordinates(x-1, y  ),                        Coordinates(x+1, y  ),
        Coordinates(x-1, y+1), Coordinates(x  , y+1), Coordinates(x+1, y+1)
    ).filter { case Coordinates(cx, cy) => inRange(cx, width) && inRange(cy, height) }
}

def findNumberSlices(mapper: Matrix ?=> (Int, Int) => Set[NumberSlice])(using matrix: Matrix): Seq[Set[NumberSlice]] =
    for {
        y <- matrix.indices
        x <- matrix(0).indices
        slices = mapper(x, y)
        if slices.nonEmpty
    } yield slices

def symbolNumbers(x: Int, y: Int)(using matrix: Matrix): Set[NumberSlice] =
    val char = matrix(y)(x)
    if !(char.isDigit || char == '.') then getNumberSlices(x, y) else Set()

def gearNumbers(x: Int, y: Int)(using matrix: Matrix): Set[NumberSlice] =
    if matrix(y)(x) == '*' then
        val gearNumbers = getNumberSlices(x, y)
        if gearNumbers.size == 2 then return gearNumbers
    Set()

def getNumberSlices(x: Int, y: Int)(using matrix: Matrix): Set[NumberSlice] =
    surrounding(x, y)
        .filter { case Coordinates(cx, cy) => matrix(cy)(cx).isDigit }
        .map(getNumberSlice)

def getNumberSlice(digitCoordinates: Coordinates)(using matrix: Matrix): NumberSlice =
    val row = matrix(digitCoordinates.y)
    val lower = { var x = digitCoordinates.x; while x > 0 && row(x-1).isDigit do x -= 1; x }
    val upper = { var x = digitCoordinates.x; while x < row.length-1 && row(x+1).isDigit do x += 1; x }
    NumberSlice(digitCoordinates.y, lower, upper)

def numberValue(numberChars: NumberSlice)(using matrix: Matrix): Int = numberChars match
    case NumberSlice(y, lower, upper) => matrix(y).slice(lower, upper+1).mkString.toInt

def gearRatio(numbers: Iterable[NumberSlice])(using matrix: Matrix): Int = numbers.map(numberValue).product

@main def main(): Unit = {

    given Matrix = input
    val result1 = findNumberSlices(symbolNumbers).flatten   //works for our input because there are no numbers with multiple symbols adjacent
        .map(numberValue)
        .sum
    println(result1)

    val result2 = findNumberSlices(gearNumbers)             //work for out input because there are no numbers with multiple gears adjacent
        .map(gearRatio)
        .sum
    println(result2)

}