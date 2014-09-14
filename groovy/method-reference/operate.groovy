def operate(Closure op) { println op(5, 9) }

class Ops {
    int add(a, b) { a + b }
    int multiply(a, b) { a * b }
}
def ops = new Ops()

operate ops.&add
operate ops.&multiply
