package day01

import scala.io.Source

val input = Source.fromResource("day01.in")
val lines: List[String] = input.getLines().toList

type DigitPair = (Int, Int)

def getDigitPair(line: String): DigitPair =
    (line.find(_.isDigit).get - '0', line.findLast(_.isDigit).get - '0')

def numericValue(digitPair: DigitPair): Int = digitPair match
    case (one, two) => 10 * one + two

def digitValue(string: String): Int = string match {
    case "0" | "zero"   => 0
    case "1" | "one"    => 1
    case "2" | "two"    => 2
    case "3" | "three"  => 3
    case "4" | "four"   => 4
    case "5" | "five"   => 5
    case "6" | "six"    => 6
    case "7" | "seven"  => 7
    case "8" | "eight"  => 8
    case "9" | "nine"   => 9
}

val searchValues = Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

def findDigit(line: String, fromIndex: Int): Option[Int] = searchValues
    .find(search => line.regionMatches(fromIndex, search, 0, search.length))
    .map(digitValue)

def getNumberPair(line: String): DigitPair =
    val pf: PartialFunction[Int, Int] = Function.unlift(findDigit(line, _))
    ((0 until line.length).collectFirst(pf).get, (line.length - 1 to 0 by -1).collectFirst(pf).get)

@main def main(): Unit = {

    val result1 = lines.map(getDigitPair).map(numericValue).sum
    println(result1)

    val result2 = lines.map(getNumberPair).map(numericValue).sum
    println(result2)

}