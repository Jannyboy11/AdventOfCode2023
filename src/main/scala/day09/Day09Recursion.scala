package day09

type Sequence = Seq[Int]

def differences(sequence: Sequence): Sequence =
    sequence.lazyZip(sequence.tail).map { case (a, b) => b - a }

def allZeroes(sequence: Sequence): Boolean = sequence.forall(_ == 0)

def predictNext(sequence: Sequence): Int =
    if allZeroes(sequence) then 0
    else sequence.last + predictNext(differences(sequence))

def predictPrevious(sequence: Sequence): Int =
    if allZeroes(sequence) then 0
    else sequence.head - predictPrevious(differences(sequence))

@main def main(): Unit = {

    val result1 = input.map(predictNext).sum
    println(result1)

    val result2 = input.map(predictPrevious).sum
    println(result2)

}