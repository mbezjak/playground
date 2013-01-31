package object complexmath {
  implicit def realToComplex(r: Double) = new ComplexNumber(r, 0.0)
  val i = ComplexNumber(0.0, 1.0)
}

package complexmath {

  case class ComplexNumber(real: Double, imaginary: Double) {
    def * (other: ComplexNumber) =
      ComplexNumber( (real * other.real) + (imaginary * other.imaginary),
                     (real * other.imaginary) + (imaginary * other.real) )

    def + (other: ComplexNumber) =
      ComplexNumber(real + other.real, imaginary + other.imaginary)
  }

}

// $ sbt console
// import complexmath.i
// val x = i * 3.0 + 1.0
// val y = 1.0 + 3.0 * i
