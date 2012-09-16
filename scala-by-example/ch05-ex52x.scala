import annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec def iter(a: Int, result: Int): Int =
    if (a > b) result else iter(a + 1, a + result)

  iter(a, 0)
}

def product(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec def iter(a: Int, result: Int): Int =
    if (a > b) result else iter(a + 1, a * result)

  iter(a, 1)
}

def factorial(n: Int): Int = product(identity)(1, n)

def foldLeft(f: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
  @tailrec def iter(a: Int, result: Int): Int =
    if (a > b) result else iter(a + 1, f(a, result))

  iter(a, init)
}

def sum2(f: Int => Int) = foldLeft({ case (a, r) => f(a) + r }, 0)_
def product2(f: Int => Int) = foldLeft({ case (a, r) => f(a) * r }, 1)_

assert(sum(identity)(1, 10) == sum2(identity)(1, 10))
assert(product(identity)(1, 10) == product2(identity)(1, 10))
