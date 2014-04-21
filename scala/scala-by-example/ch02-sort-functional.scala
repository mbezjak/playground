def sort(xs: Array[Int]): Array[Int] = {
  if (xs.length <= 1) xs
  else {
    val pivot = xs(xs.length / 2)
    Array.concat(
      sort(xs filter (pivot >)),
      xs filter (pivot ==),
      sort(xs filter (pivot <)))
  }
}

def data = Array(1, 9, 5, 6, 2, 7, 8, 4, 3)

println(data.mkString(", "))
println(sort(data).mkString(", "))
