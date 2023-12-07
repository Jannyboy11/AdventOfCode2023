package day07

import day07.WinCondition.HighCard

import scala.annotation.switch
import scala.collection.SeqView.Sorted
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

enum Rank(val char: Char):
    case Two extends Rank('2')
    case Three extends Rank('3')
    case Four extends Rank('4')
    case Five extends Rank('5')
    case Six extends Rank('6')
    case Seven extends Rank('7')
    case Eight extends Rank('8')
    case Nine extends Rank('9')
    case Ten extends Rank('T')
    case Jack extends Rank('J')
    case Queen extends Rank('Q')
    case King extends Rank('K')
    case Ace extends Rank('A')

enum WinCondition:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

    object OnePair:
        def is(sortedHand: Hand): Boolean = sortedHand match
            case Seq(a, b, c, d, e) if a == b || b == c || c == d || d == e => true
            case _ => false
    object TwoPair:
        def is(sortedHand: Hand): Boolean = sortedHand match
            case Seq(a, b, c, d, e) if (a == b && (c == d || d == e)) || (a == c && d == e) => true
            case _ => false
    object ThreeOfAKind:
        def is(sortedHand: Hand): Boolean =
            allEqual(sortedHand.head, sortedHand.take(3)) || allEqual(sortedHand.last, sortedHand.drop(2))
    object FullHouse:
        def is(sortedHand: Hand): Boolean =
            (allEqual(sortedHand.head, sortedHand.take(3)) && allEqual(sortedHand.last, sortedHand.drop(3))) ||
                (allEqual(sortedHand.head, sortedHand.take(2)) && allEqual(sortedHand.last, sortedHand.drop(2)))
    object FourOfAKind:
        def is(sortedHand: Hand): Boolean =
            allEqual(sortedHand.head, sortedHand.take(4)) || allEqual(sortedHand.last, sortedHand.drop(1))
    object FiveOfAKind:
        def is(sortedHand: Hand): Boolean = allEqual(sortedHand.head, sortedHand)

def sortHand(hand: Hand): Hand = hand.sortBy(_.ordinal)(using summon[Ordering[Int]].reverse)

def allEqual[A](to: A, coll: Iterable[A]): Boolean = coll.forall(_ == to)

def winCondition(sortedHand: Hand): WinCondition = {
    import WinCondition.*
    //TODO can we use typeclass derivation instead?
    if FiveOfAKind.is(sortedHand) then
        FiveOfAKind
    else if FourOfAKind.is(sortedHand) then
        FiveOfAKind
    else if FullHouse.is(sortedHand) then
        FullHouse
    else if ThreeOfAKind.is(sortedHand) then
        ThreeOfAKind
    else if TwoPair.is(sortedHand) then
        TwoPair
    else if OnePair.is(sortedHand) then
        OnePair
    else
        HighCard
}

@main def main(): Unit = {

    val result1 = ???
    println(result1)

    val result2 = ???
    println(result2)

}