// --- basic -----------------------------------------------
def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts (a + 1, b)

def square(x: Int): Int = x * x
def sumSquares(a: Int, b: Int): Int =
  if (a > b) 0 else square(a) + sumSquares(a + 1, b)

def powerOfTwo(x: Int): Int =
  if (x == 0) 1 else 2 * powerOfTwo(x - 1)
def sumPowersOfTwo(a: Int, b: Int): Int =
  if (a > b) 0 else powerOfTwo(a) + sumPowersOfTwo(a + 1, b)

// --- higher-order ----------------------------------------
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

def sumInts2(a: Int, b: Int): Int = sum(identity, a, b)
def sumSquares2(a: Int, b: Int): Int = sum(square, a, b)
def sumPowersOfTwo2(a: Int, b: Int): Int = sum(powerOfTwo, a, b)

assert(sumInts(1, 5) == sumInts2(1, 5))
assert(sumSquares(1, 5) == sumSquares2(1, 5))
assert(sumPowersOfTwo(1, 5) == sumPowersOfTwo2(1, 5))

// --- anonymous -------------------------------------------
def sumInts3(a: Int, b: Int): Int = sum(x => x, a, b)
def sumSquares3(a: Int, b: Int): Int = sum(x => x * x, a, b)

assert(sumInts(1, 5) == sumInts3(1, 5))
assert(sumSquares(1, 5) == sumSquares3(1, 5))

// --- curried ---------------------------------------------
def sum4(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumF(a + 1, b)
  }
  sumF
}

def sumInts4 = sum4(identity)
def sumSquares4 = sum4(square)
def sumPowersOfTwo4 = sum4(powerOfTwo)

assert(sumInts(1, 5) == sumInts4(1, 5))
assert(sumSquares(1, 5) == sumSquares4(1, 5))
assert(sumPowersOfTwo(1, 5) == sumPowersOfTwo4(1, 5))

// --- curried - in scala syntax----------------------------
def sum5(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum5(f)(a + 1, b)

def sumInts5 = sum5(identity)_
def sumSquares5 = sum5(square)_
def sumPowersOfTwo5 = sum5(powerOfTwo)_

assert(sumInts(1, 5) == sumInts5(1, 5))
assert(sumSquares(1, 5) == sumSquares5(1, 5))
assert(sumPowersOfTwo(1, 5) == sumPowersOfTwo5(1, 5))
