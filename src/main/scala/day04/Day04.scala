package day04

import scala.io.Source

val source = Source.fromResource("day04.in")
val input: Scratchpad = source.getLines().map(line => {
    val id = line.substring(5, 8).trim.toInt
    val winning = Range.inclusive(10, 37, 3).map(idx => line.substring(idx, idx+2).trim.toInt).toSet
    val have = Range.inclusive(42, 114, 3).map(idx => line.substring(idx, idx+2).trim.toInt).toSet
    Card(id, winning, have)
}).toIndexedSeq

type ID = Int
type Count = Int
case class Card(id: ID, winning: Set[Int], have: Set[Int])
type Scratchpad = IndexedSeq[Card]
type CardCounts = Map[ID, Count]

def points(card: Card): Int = points(overlapping(card))

def overlapping(card: Card): Int = card match
    case Card(_, winning, have) => winning.intersect(have).size

def points(overlapping: Int): Int =
    LazyList.iterate(1)(_ * 2).take(overlapping).lastOption.getOrElse(0)

def count(scratchpad: Scratchpad): CardCounts = scratchpad.map(card => (card.id, 1)).toMap

def process(id: ID, counts: CardCounts, scratchpad: Scratchpad): CardCounts =
    val card = scratchpad(id - 1)
    val count = counts(id)
    Range.inclusive(id + 1, id + overlapping(card)).foldLeft(counts)((newCounts, otherId) => process(otherId, newCounts, count))

def process(id: ID, counts: CardCounts, add: Count): CardCounts =
    counts.updatedWith(id) { case Some(oldCount) => Some(oldCount + add) }

@main def main(): Unit = {

    val result1 = input.map(points).sum
    println(result1)

    val result2 = input.foldLeft(count(input)) { case (acc, card) => process(card.id, acc, input) }.values.sum
    println(result2)

}