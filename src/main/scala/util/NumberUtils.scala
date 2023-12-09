package util

import scala.annotation.tailrec

@tailrec
def gcd(a: BigInt, b: BigInt): BigInt = if b == 0 then a else gcd(b, a % b)

def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)