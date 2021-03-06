import annotation.tailrec

@tailrec
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

def factorial(n: Int): Int = {
  @tailrec def iter(n: Int, r: Int): Int =
    if (n == 0) r else iter(n-1, r * n)

  iter(n, 1)
}
