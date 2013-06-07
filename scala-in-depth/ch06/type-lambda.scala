def foo[M[_]](f: M[Int]) = f

type Callback[T] = Function1[T, Unit]
val x: Callback[Int] = x => println(x)

foo[Callback](x)
//foo[Function1](x)

foo[({type L[X] = Function1[X,Unit]})#L](x)
