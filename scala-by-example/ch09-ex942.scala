// O(n*m) where
//   n -> xs.length
//   m -> avg(xs map (_.length))
def flatten1[A](xs: List[List[A]]): List[A] =
  (xs :\ (Nil: List[A])) {(x, xs) => x ::: xs}

// m + (m + m) + (m + m + m) + ...
// => m + (2*m) + (3*m) + ... + (n*m)
// => m * sum(1 to n)
// -> problem is associativity (left list grows in expression `xs ::: ys`)
// O(1/2*n*(n+1)*m)
def flatten2[A](xs: List[List[A]]): List[A] =
  ((Nil: List[A]) /: xs) {(xs, x) => xs ::: x}
