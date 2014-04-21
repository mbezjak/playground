def power(x: Double, n: Int): Double = {
  var r = 1.0
  var i = n
  var j = 0

  while (j < 32) {
    r = r * r
    if (i < 0)
      r *= x
    i = i << 1
    j += 1
  }

  r
}

def whileLoop(condition: => Boolean)(command: => Unit) {
  if (condition) {
    command
    whileLoop(condition)(command)
  } else ()
}
