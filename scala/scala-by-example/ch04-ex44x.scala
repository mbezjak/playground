def square(x: Double) = x * x
def abs(x: Double)    = if (x >= 0) x else -x

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def isGoodEnough(guess: Double, x: Double) =
  abs(square(guess) - x) < 0.001

def isGoodEnoughImproved(guess: Double, x: Double) =
  abs(square(guess) - x) / x <= 1e-3

def sqrtIter(guess: Double, x: Double): Double = {
  if (isGoodEnoughImproved(guess, x)) guess
  else sqrtIter(improve(guess, x), x)
}

def sqrt(x: Double) = sqrtIter(1.0, x)

/*
 * sqrt(4)
 *
 * 1            4/1 = 4            2.5
 * 2.5          4/2.5 = 1.6        2.05
 * 2.05         4/2.05 = 1.95      2
 * -----------------------------------------
 * y            x/y                (y+x/y)/2
 */
