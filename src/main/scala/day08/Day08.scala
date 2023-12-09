package day08

import scala.annotation.tailrec
import scala.io.Source
import util.lcm

val source = Source.fromResource("day08.in")
val (instructions: Seq[Instruction], network: Network) = {
    val iterator = source.getLines()
    val instructions = iterator.next().map(Instruction.parse)
    iterator.next()
    val network = iterator.map { case s"${node} = (${nextLeft}, ${nextRight})" => node -> (nextLeft, nextRight) }.toMap
    (instructions, network)
}

type State = String
type Network = Map[State, (State, State)]
object Instruction:
    def parse(char: Char): Instruction = char match
        case 'L' => Left
        case 'R' => Right
enum Instruction:
    case Left
    case Right

def choose(instruction: Instruction, choices: (State, State)): State = (instruction, choices) match
    case (Instruction.Left, (nextLeft, _)) => nextLeft
    case (Instruction.Right, (_, nextRight)) => nextRight

def step(state: State, instruction: Instruction, network: Network): State =
    choose(instruction, network(state))

def cycle[I](items: Seq[I]): LazyList[I] =
    LazyList.continually(items).flatten

@tailrec
def follow[T](instructions: LazyList[Instruction], state: State, network: Network, sideValue: T, update: T => T, break: State => Boolean): T =
    if break(state) then
        sideValue
    else
        val head +: tail = instructions: @unchecked
        follow(tail, step(state, head, network), network, update(sideValue), update, break)

@main def main(): Unit = {

    val result1 = follow(cycle(instructions), "AAA", network, 0, _ + 1, _ == "ZZZ")
    println(result1)

    val result2 = {
        val initialStates: Seq[State] = network.keys.filter(_.endsWith("A")).toSeq
        val durations: Seq[BigInt] = initialStates.map(state => follow(cycle(instructions), state, network, BigInt(0), _ + 1, _.endsWith("Z")))
        durations.reduce(lcm)
    }
    println(result2)

}