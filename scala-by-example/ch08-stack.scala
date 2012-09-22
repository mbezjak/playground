object holder {

  abstract class Stack[+A] {
    def push[B >: A](x: B): Stack[B] = new NonEmptyStack[B](x, this)
    def isEmpty: Boolean
    def top: A
    def pop: Stack[A]
  }

  object EmptyStack extends Stack[Nothing] {
    def isEmpty = true
    def top = sys.error("EmptyStack.top")
    def pop = sys.error("EmptyStack.pop")
  }

  class NonEmptyStack[A](x: A, rest: Stack[A]) extends Stack[A] {
    def isEmpty = false
    def top = x
    def pop = rest
  }

  def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean =
    p.isEmpty || (p.top == s.top && isPrefix(p.pop, s.pop))

}

import holder._
