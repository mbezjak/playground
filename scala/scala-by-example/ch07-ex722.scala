abstract class IntTree
case object EmptyTree extends IntTree
case class  Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

def contains(t: IntTree, v: Int): Boolean = t match {
  case EmptyTree     => false
  case Node(x, l, r) => v == x || contains(l, v) || contains(r, v)
}

def insert(t: IntTree, v: Int): IntTree = t match {
  case EmptyTree     => Node(v, EmptyTree, EmptyTree)
  case Node(e, l, r) =>
    if (v < e)      Node(e, insert(l, v), r)
    else if (v > e) Node(e, l, insert(r, v))
    else            Node(e, l, r)
}
