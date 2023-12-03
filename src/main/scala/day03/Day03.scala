package day03

import java.util.concurrent.locks.Condition
import scala.annotation.switch
import scala.io.Source

val source = Source.fromResource("day03.in")
val input: Matrix = source.getLines().map(_.toIndexedSeq).toIndexedSeq
type Matrix = IndexedSeq[IndexedSeq[Char]]
case class Coordinates(x: Int, y: Int)
case class NumberSlice(y: Int, xLower: Int, xUpper: Int)

def isSymbol(char: Char): Boolean = !(char.isDigit || char == '.')

def surrounding(matrix: Matrix, x: Int, y: Int): Set[Coordinates] = {
    val height = matrix.length
    val width = matrix(0).length
    def inRange(i: Int, upperEx: Int): Boolean = 0 <= i && i < upperEx
    Set(
        Coordinates(x-1, y-1), Coordinates(x  , y-1), Coordinates(x+1, y-1),
        Coordinates(x-1, y  ),                        Coordinates(x+1, y  ),
        Coordinates(x-1, y+1), Coordinates(x  , y+1), Coordinates(x+1, y+1)
    ).filter { case Coordinates(cx, cy) => inRange(cx, width) && inRange(cy, height) }
}

def findNumberSlices(matrix: Matrix, mapper: (Matrix, Int, Int) => Set[NumberSlice]): Map[Coordinates, Set[NumberSlice]] = {
    val entries: Seq[(Coordinates, Set[NumberSlice])] = for {
        y <- matrix.indices
        x <- matrix(0).indices
        slices = mapper(matrix, x, y)
        if slices.nonEmpty
    } yield (Coordinates(x, y), slices)
    entries.toMap
}

def symbolNumbers(matrix: Matrix, x: Int, y: Int): Set[NumberSlice] =
    if isSymbol(matrix(y)(x)) then getNumberSlices(matrix, x, y) else Set()

def gearNumbers(matrix: Matrix, x: Int, y: Int): Set[NumberSlice] =
    if matrix(y)(x) == '*' then
        val gearNumbers = getNumberSlices(matrix, x, y)
        if gearNumbers.size == 2 then return gearNumbers
    Set()

def getNumberSlices(matrix: Matrix, x: Int, y: Int): Set[NumberSlice] = surrounding(matrix, x, y)
    .filter { case Coordinates(cx, cy) => matrix(cy)(cx).isDigit }
    .map(getNumberSlice(matrix, _))

def getNumberSlice(matrix: Matrix, digitCoordinates: Coordinates): NumberSlice =
    val row = matrix(digitCoordinates.y)
    val lower = { var x = digitCoordinates.x; while x > 0 && row(x-1).isDigit do x -= 1; x }
    val upper = { var x = digitCoordinates.x; while x < row.length-1 && row(x+1).isDigit do x += 1; x }
    NumberSlice(digitCoordinates.y, lower, upper)

def numberValue(matrix: Matrix, numberChars: NumberSlice): Int = numberChars match
    case NumberSlice(y, lower, upper) => matrix(y).slice(lower, upper+1).mkString.toInt

def gearRatio(matrix: Matrix, numbers: Seq[NumberSlice]): Int = numbers.map(numberValue(matrix, _)).product

@main def main(): Unit = {

    val result1 = findNumberSlices(input, symbolNumbers)
        .flatMap { case (_, numbers) => numbers }.toSeq.distinct
        .map(numberValue(input, _))
        .sum
    println(result1)

    val result2 = findNumberSlices(input, gearNumbers).values
        .map(numberSlices => gearRatio(input, numberSlices.toSeq))
        .sum
    println(result2)

}