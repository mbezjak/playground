case class Book(title: String, authors: List[String])

def removeDuplicates[A](xs: List[A]): List[A] = xs match {
  case List()  => Nil
  case y :: ys => y :: removeDuplicates(for (z <- ys if z != y) yield z)
}

val books: List[Book] = List(
  Book("Structure and Interpretation of Computer Programs",
       List("Abelson, Harold", "Sussman, Gerald J.")),
  Book("Principles of Compiler Design",
       List("Aho, Alfred", "Ullman, Jeffrey")),
  Book("Programming in Modula-2",
       List("Wirth, Niklaus")),
  Book("Introduction to Functional Programming",
       List("Bird, Richard")),
  Book("The Java Language Specification",
       List("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")),
  Book("Common LISP. The Language.",
       List("Steele, Guy")))

val ullman = for (b <- books; a <- b.authors if a startsWith "Ullman")
             yield b.title

val program = for (b <- books if b.title.contains("Program"))
              yield b.title

val two = for (b1 <- books; b2 <- books if b1 != b2;
               a1 <- b1.authors; a2 <- b2.authors if a1 == a2)
          yield a1

val atLeastTwo = removeDuplicates(two)


def flatten[A](xss: List[List[A]]): List[A] =
  (xss :\ (Nil: List[A])) ((xs, ys) => xs ::: ys)

def flatten2[A](xss: List[List[A]]): List[A] =
  for (xs <- xss; x <- xs) yield x

val bird = books.flatMap (b => b.authors filter (_ startsWith "Bird") map (a => b.title))
val program2 = books filter (_.title contains "Program") map (_.title)
