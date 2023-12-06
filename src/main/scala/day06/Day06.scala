package day06

val times =     Seq(44, 70, 70, 80)
val distances = Seq(283, 1134, 1134, 1491)

def ways(raceTime: Int, distanceToBeat: Int): Int =
    Range.inclusive(0, raceTime).count(buttonTime => distance(buttonTime, raceTime) > distanceToBeat)

def distance(buttonTime: Int, totalTime: Int): Int =
    val travelTime = totalTime - buttonTime
    buttonTime/*speed*/ * travelTime

val time     =        44707080L
val distance = 283113411341491L

//we have the following equation: distanceTravelled = buttonTime*(totalTime-buttonTime)
//which is equivalent to                            = -buttonTime^2 + buttonTime*totalTime

//now we have to solve the equation: -buttonTime^2 + totalTime*buttonTime > goalDistance
//                                   -buttonTime^2 + totalTime*buttonTime - goalDistance > 0
//and measure the length between the solutions.
//to get the solutions we solve for -buttonTime^2 + totalTime*buttonTime - goalDistance == 0.

def discriminant(a: Long, b: Long, c: Long): Long = b*b - 4*a*c
def quadraticFormula(a: Long, b: Long, c: Long): Seq[Double] =
    val D = discriminant(a, b, c)
    if D < 0 then
        Seq()
    else
        Seq(
            (-b + Math.sqrt(D)) / (2*a),
            (-b - Math.sqrt(D)) / (2*a),
        )

@main def main(): Unit = {

    val result1 = times.zip(distances).map(ways).product
    println(result1)

    val result2 = {
        val a = -1
        val b = time
        val c = -distance
        val Seq(lower, upper) = quadraticFormula(a, b, c)
        val lowerRoot = lower.ceil.toLong       //some rounding needed because we can only hold the button for integer number of ms.
        val upperRoot = upper.floor.toLong + 1  //+1 because the number of combinations *includes* the second solution.
        upperRoot - lowerRoot
    }
    println(result2)

}