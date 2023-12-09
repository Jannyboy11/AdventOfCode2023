package day07

import day07.WinCondition.HighCard

import scala.annotation.switch
import scala.collection.SeqView.Sorted
import scala.collection.mutable
import scala.deriving.Mirror
import scala.io.Source

val source = Source.fromResource("day07.in")
val input: Seq[(Hand, Bid)] = source.getLines().map { case s"$hand $bid" => (hand.map(Rank.parse), bid.toInt) }.toSeq

type Hand = Seq[Rank]
type Bid = Int

object Rank:
    def parse(char: Char): Rank = (char: @switch) match
        case '2' => Two
        case '3' => Three
        case '4' => Four
        case '5' => Five
        case '6' => Six
        case '7' => Seven
        case '8' => Eight
        case '9' => Nine
        case 'T' => Ten
        case 'J' => Jack
        case 'Q' => Queen
        case 'K' => King
        case 'A' => Ace

enum Rank:
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace

enum WinCondition extends java.lang.Enum[WinCondition]:
    //extend java.lang.Enum, so that this class implements Comparable
    //when then in turn makes the Ordering[WinCondition] instance implicitly available :-)
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

def allEqual[A](to: A, coll: Iterable[A]): Boolean = coll.forall(_ == to)

object OnePair:
    def is(sortedHand: Hand): Boolean = sortedHand match
        case Seq(a, b, c, d, e) if a == b || b == c || c == d || d == e => true
        case _ => false
object TwoPair:
    def is(sortedHand: Hand): Boolean =
        sortedHand match
            case Seq(a, b, c, d, e) if (a == b && (c == d || d == e)) || (a == c && d == e) || (b == c && d == e) =>  true
            case _ => false
object ThreeOfAKind:
    def is(sortedHand: Hand): Boolean =
        allEqual(sortedHand(0), sortedHand.take(3)) || allEqual(sortedHand(1), sortedHand.slice(1, 4)) || allEqual(sortedHand(2), sortedHand.slice(2, 5))
object FullHouse:
    def is(sortedHand: Hand): Boolean =
        (allEqual(sortedHand.head, sortedHand.take(3)) && allEqual(sortedHand.last, sortedHand.drop(3))) ||
            (allEqual(sortedHand.head, sortedHand.take(2)) && allEqual(sortedHand.last, sortedHand.drop(2)))
object FourOfAKind:
    def is(sortedHand: Hand): Boolean =
        allEqual(sortedHand.head, sortedHand.take(4)) || allEqual(sortedHand.last, sortedHand.drop(1))
object FiveOfAKind:
    def is(sortedHand: Hand): Boolean = allEqual(sortedHand.head, sortedHand)

def sortHand(hand: Hand)(using Ordering[Rank]): Hand = hand.sorted

def winConditionP1(sortedHand: Hand): WinCondition = {
    if FiveOfAKind.is(sortedHand) then
        WinCondition.FiveOfAKind
    else if FourOfAKind.is(sortedHand) then
        WinCondition.FourOfAKind
    else if FullHouse.is(sortedHand) then
        WinCondition.FullHouse
    else if ThreeOfAKind.is(sortedHand) then
        WinCondition.ThreeOfAKind
    else if TwoPair.is(sortedHand) then
        WinCondition.TwoPair
    else if OnePair.is(sortedHand) then
        WinCondition.OnePair
    else
        WinCondition.HighCard
}

def winConditionP2(sortedHand: Hand): WinCondition =
    if allEqual(Rank.Jack, sortedHand) then return WinCondition.FiveOfAKind
    val highestMostOccurringRank: Rank = highestMostOccurring(sortedHand.filter(_ != Rank.Jack))
    val p1Hand = sortedHand.map(rank => if rank == Rank.Jack then highestMostOccurringRank else rank)
    winConditionP1(sortHand(p1Hand)(using individualOrderingP2))

def highestMostOccurring(handWithoutJokers: Hand): Rank = {
    val counts = scala.collection.mutable.Map[Rank, Int]()
    for rank <- handWithoutJokers do
        counts.updateWith(rank) { case Some(count) => Some(count + 1); case None => Some(1) }
    val pairOrdering: Ordering[(Rank, Int)] = Ordering.by[(Rank, Int), Int] { case (_, count) => count }
        .orElse(Ordering.by[(Rank, Int), Rank] { case (rank, _) => rank }(using individualOrderingP2))
    val (mostOccurringRank, _) = counts.max(using pairOrdering)
    mostOccurringRank
}

def sortedHands(hands: Seq[Hand]): Map[Hand, Hand] =
    hands.map(hand => (hand, sortHand(hand)(using individualOrderingP1))).toMap

def handOrdering(using individualOrdering: Ordering[Rank], winCondition: Hand => WinCondition): Ordering[Hand] =
    Ordering.by[Hand, WinCondition](hand => winCondition(sortHand(hand)))
        .orElse(Ordering.Implicits.seqOrdering(using individualOrdering))

def makeRankings(cards: Seq[Hand])(using handOrdering: Ordering[Hand]): Map[Hand, Int] =
    cards.sorted.zip(LazyList.iterate(1)(_ + 1)).toMap

val individualOrderingP1: Ordering[Rank] = Ordering.by(_.ordinal)

val individualOrderingP2: Ordering[Rank] = Ordering.fromLessThan { (one, two) =>
    if one == Rank.Jack then two != Rank.Jack else if two == Rank.Jack then false else one.ordinal < two.ordinal
}

val handOrderingP1: Ordering[Hand] = handOrdering(using individualOrderingP1, winConditionP1)

val handOrderingP2: Ordering[Hand] = handOrdering(using individualOrderingP2, winConditionP2)

def solve(input: Seq[(Hand, Bid)])(using Ordering[Hand]): Long =
    val hands = input.map((hand, bid) => hand)
    val rankings = makeRankings(hands)
    input.map((hand, bid) => (rankings(hand) * bid).toLong).sum

@main def main(): Unit = {

    val result1 = solve(input)(using handOrderingP1)
    println(result1)

    val result2 = solve(input)(using handOrderingP2)
    println(result2)

}