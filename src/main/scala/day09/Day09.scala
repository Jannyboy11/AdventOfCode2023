package day09

import scala.io.Source

val source = Source.fromResource("day09.in")
val input: Seq[Seq[Int]] = source.getLines().map(line => line.split(" ").map(_.toInt).toSeq).toSeq

def lagrangeInterpolate(data: Seq[Int])(xi: Int): BigDecimal = {
    val n = data.length
    var result: BigDecimal = 0D

    for i <- 0 until n do
        var term = BigDecimal(data(i))
        for j <- 0 until n do
            if j != i then
                term = term * BigDecimal(xi - j) / BigDecimal(i - j)
        result += term

    result
}

@main def lagrangeMain(): Unit = {

    val polynomials: Seq[Int => BigDecimal] = input.map(lagrangeInterpolate)

    val result1 = {
        val lengths: Seq[Int] = input.map(_.length)
        polynomials.lazyZip(lengths).map((f, length) => f(length)).sum.toInt
    }
    println(result1)

    val result2 = polynomials.map(f => f(-1)).sum.toInt
    println(result2)

}