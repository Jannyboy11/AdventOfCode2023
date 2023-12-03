package day02

import scala.io.Source

val source = Source.fromResource("day02.in")
val input: List[Game] = source.getLines().map { case s"Game ${id}: ${reveal}" =>
    val revealsStrings: Array[String] = reveal.split("; ")
    val reveals: Seq[Reveal] = revealsStrings.map(_.split(", ").map { case s"${count} ${colour}" => Cubes(count.toInt, colour) } .toSet).toSeq
    Game(id.toInt, reveals)
} .toList

case class Game(id: Int, reveals: Seq[Reveal])
type Reveal = Set[Cubes]
type Colour = String
type Count  = Int
case class Cubes(count: Count, colour: Colour)

type Bag            = Map[Colour, Count]
val bag1: Bag       = Map("red" -> 12, "green" -> 13, "blue" -> 14)
val emptyBag: Bag   = Map.empty

def possible(bag: Bag, game: Game): Boolean =
    game.reveals.forall(possible(bag, _))

def possible(bag: Bag, reveal: Reveal): Boolean =
    reveal.forall(possible(bag, _))

def possible(bag: Bag, cubes: Cubes): Boolean =
    bag.getOrElse(cubes.colour, 0) >= cubes.count

def minimumBag(game: Game): Bag =
    game.reveals.foldLeft(emptyBag)(minimumBag)

def minimumBag(currentMinimum: Bag, reveal: Reveal): Bag =
    reveal.foldLeft(currentMinimum)((bag, cube) => bag.updatedWith(cube.colour) {
        case Some(min)  => Some(Math.max(min, cube.count))
        case None       => Some(cube.count)
    })

def power(bag: Bag): Int = bag("red") * bag("green") * bag("blue")

@main def main(): Unit = {

    val result1 = input.filter(possible(bag1, _)).map(_.id).sum
    println(result1)

    val result2 = input.map(minimumBag).map(power).sum
    println(result2)

}