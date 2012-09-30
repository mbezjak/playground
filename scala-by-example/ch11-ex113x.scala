type Action = () => Unit

val InverterDelay = 10
val AndGateDelay  = 10
val OrGateDelay   = 10

class Wire {
  private var sigVal = false
  private var actions: List[Action] = List()

  def getSignal = sigVal
  def setSignal(s: Boolean) =
    if (s != sigVal) {
      sigVal = s
      actions foreach (action => action())
    }

  def addAction(a: Action) {
    actions = a :: actions
    a()
  }
}

class Simulation {
  case class WorkItem(time: Int, action: Action)
  private type Agenda = List[WorkItem]
  private var agenda: Agenda = List()
  private var curtime = 0

  private def insert(ag: Agenda, item: WorkItem): Agenda =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)

  def afterDelay(delay: Int)(block: => Unit) {
    val item = WorkItem(curtime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() {
    agenda match {
      case WorkItem(time, action) :: rest =>
        agenda = rest; curtime = time; action()
      case List() =>
    }
  }

  def run() {
    afterDelay(0) { println("*** simulation started ***") }
    while (!agenda.isEmpty) next()
  }



  def probe(name: String, wire: Wire) {
    wire addAction { () =>
      println(name + " " + curtime + " new_value = " + wire.getSignal)
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSig }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output setSignal (a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output setSignal (a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  // delay = AndGateDelay + InverterDelay
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val b = new Wire
    andGate(a1, a2, b)
    inverter(b, output)
  }

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) {
    val s  = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }

}

val s = new Simulation
import s._

val input1, input2, sum, carry = new Wire
probe("sum", sum)
probe("carry", carry)
halfAdder(input1, input2, sum, carry)

println("-" * 50)
input1 setSignal true
run

input2 setSignal true
run
