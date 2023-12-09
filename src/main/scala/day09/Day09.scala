package day09

import scala.io.Source

val source = Source.fromResource("day09.in")
val input: Seq[Sequence] = source.getLines().map(line => line.split(" ").map(_.toLong).toSeq).toSeq

type Sequence = Seq[Long]

def differences(sequence: Sequence): Sequence =
    sequence.zip(sequence.tail).map { case (a, b) => b - a }

def allZeroes(sequence: Sequence): Boolean = sequence.forall(_ == 0)

def predictNext(sequence: Sequence): Long =
    if allZeroes(sequence) then 0L
    else sequence.last + predictNext(differences(sequence))

def predictPrevious(sequence: Sequence): Long =
    if allZeroes(sequence) then 0L
    else sequence.head - predictPrevious(differences(sequence))

@main def main(): Unit = {

    val result1 = input.map(predictNext).sum
    println(result1)

    val result2 = input.map(predictPrevious).sum
    println(result2)

}