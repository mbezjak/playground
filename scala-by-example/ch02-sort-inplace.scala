def sort(xs: Array[Int]) {
  def swap(i: Int, j: Int) {
    val x = xs(i)
    xs(i) = xs(j)
    xs(j) = x
  }
  def sort1(l: Int, r: Int) {
    val pivot = xs((r + l) / 2)
    var i = l
    var j = r
    while (i <= j) {
      while (xs(i) < pivot) i += 1
      while (xs(j) > pivot) j -= 1
      if (i <= j) {
        swap(i, j)
        i += 1
        j -= 1
      }
    }
    if (l < j) sort1(l, j)
    if (j < r) sort1(i, r)
  }

  if (!xs.isEmpty)
    sort1(0, xs.length - 1)
}

def data = Array(1, 9, 5, 6, 2, 7, 8, 4, 3)

val a = data
println(a.mkString(", "))
sort(a)
println(a.mkString(", "))
