// implementation is do-while because of ordering of arguments
def repeatLoop(command: => Unit)(condition: => Boolean) {
  command
  if (condition) repeatLoop(command)(condition)
}

def repeatLoop2(command: => Unit) = new {
  def until(condition: => Boolean) {
    command
    if (condition) until(condition)
  }
}

var x = 5
repeatLoop { x -= 1 } (x > 0)
assert(x == 0)

var x = 5
repeatLoop2 { x -= 1 } until (x > 0)
assert(x == 0)
