sealed trait TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] <: Up
}

class TTrue extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
}

class TFalse extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
}

def test {
  type X[T <: TBool] = T#If[String, Int, Any]

  val a : X[TTrue]  = "a"
  val b : X[TFalse] = 5

  //val c : X[TTrue]  = 5
}
