object holder {

  def queens(n: Int): List[List[Int]] = {
    def placeQueens(k: Int): List[List[Int]] =
      if (k == 0) List(List())
      else for { queens <- placeQueens(k - 1)
        column <- List.range(1, n + 1)
        if isSafe(column, queens, 1) } yield column :: queens

    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int], delta: Int): Boolean = {
    def isSafeFromQueen(queen: Int): Boolean =
      col != queen && queen + delta != col && queen - delta != col

    queens match {
      case List()  => true
      case q :: qs => isSafeFromQueen(q) && isSafe(col, qs, delta+1)
    }
  }

}

import holder._

// 4x4 solution #1
// +---+---+---+---+
// |   |   | x |   |
// +---+---+---+---+
// | x |   |   |   |
// +---+---+---+---+
// |   |   |   | x |
// +---+---+---+---+
// |   | x |   |   |
// +---+---+---+---+
//
// 4x4 solution #2
// +---+---+---+---+
// |   | x |   |   |
// +---+---+---+---+
// |   |   |   | x |
// +---+---+---+---+
// | x |   |   |   |
// +---+---+---+---+
// |   |   | x |   |
// +---+---+---+---+
