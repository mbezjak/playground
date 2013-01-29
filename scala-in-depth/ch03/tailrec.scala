import scala.annotation.tailrec

case class Node(name: String, edges: List[Node] = Nil)

def search(start: Node, p: Node => Boolean): Option[Node] = {
  @tailrec
  def loop(nodeQueue: List[Node], visited: Set[Node]): Option[Node] = nodeQueue match {
    case Nil   => None
    case x::xs if p(x) => Some(x)
    case x::xs if !visited.contains(x) => loop(xs ++ x.edges, visited + x)
    case x::xs => loop(xs, visited)
  }

  loop(List(start), Set())
}

val root = Node("root", List(
  Node("a1"), Node("a2"), Node("a3", List(Node("b1"))),
  Node("z4"), Node("z5")))
