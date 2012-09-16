import annotation.tailrec
import math.abs

val tolerance = 1e-4
def isCloseEnough(x: Double, y: Double): Boolean =
  abs((x - y) / x) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  @tailrec def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)
def cube(x: Double) = fixedPoint(averageDamp(y => x/(y*y)))(1.0)
