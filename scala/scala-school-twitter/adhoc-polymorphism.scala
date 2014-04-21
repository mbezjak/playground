trait Container[M[_]] {
  def put[A](x: A): M[A]
  def get[A](m: M[A]): A
}

implicit val listContainer: Container[List] = new Container[List] {
  def put[A](x: A): List[A] = List(x)
  def get[A](m: List[A]): A = m.head
}

implicit val someContainer: Container[Some] = new Container[Some] {
  def put[A](x: A): Some[A] = Some(x)
  def get[A](m: Some[A]): A = m.head
}

def tupleize[M[_] : Container, A, B](fst: M[A], snd: M[B]): M[(A,B)] = {
  val c = implicitly[Container[M]]
  c put (c.get(fst) -> c.get(snd))
}

def map[M[_] : Container, A, B](m: M[A])(f: A => B): M[B] = {
  val c = implicitly[Container[M]]
  c put f(c get m)
}
