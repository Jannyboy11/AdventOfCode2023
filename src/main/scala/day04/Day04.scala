package day04

import scala.io.Source

val source = Source.fromResource("day04.in")
val input: List[Card] = source.getLines().map(line => {
    val id = line.substring(5, 8).trim.toInt
    val winning = Range.inclusive(10, 37, 3).map(idx => line.substring(idx, idx+2).trim.toInt)
    val have = Range.inclusive(42, 114, 3).map(idx => line.substring(idx, idx+2).trim.toInt)
    Card(id, winning, have)
}).toList

case class Card(id: Int, winning: Seq[Int], have: Seq[Int])

@main def main(): Unit = {

    val result1 = ???
    println(result1)

    val result2 = ???
    println(result2)

}