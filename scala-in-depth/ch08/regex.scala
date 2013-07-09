val P = "([^=]+)=(.+)".r

"a = 1" match {
  case P(k,v) => println(s"$k = $v")
  case _      => println("no match")
}
