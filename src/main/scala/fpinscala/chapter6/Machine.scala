package fpinscala.chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

final case class Machine(locked: Boolean = true, candies: Int, coins: Int)

object Machine {

  type TInt = (Int, Int)

  def simulateMachineT(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def modify(input: Input): Machine => Machine = machine => input match {
      case Coin =>
        if (machine.candies == 0 || !machine.locked) machine
        else machine.copy(locked = false, coins = machine.coins + 1)
      case Turn =>
        if (machine.candies == 0 || machine.locked) machine
        else machine.copy(locked = true, candies = machine.candies - 1)
    }

    for {
      _ <- State.traverse(inputs)(State.modify[Machine] _ compose modify)
      machine <- State.get
    } yield (machine.coins, machine.candies)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
    val machine2 = inputs.foldLeft(machine)((machine, input) => input match {
      case Coin =>
        if (machine.candies == 0 || !machine.locked) machine
        else machine.copy(locked = false, coins = machine.coins + 1)
      case Turn =>
        if (machine.candies == 0 || machine.locked) machine
        else machine.copy(locked = true, candies = machine.candies - 1)
    })

    ((machine2.coins, machine2.candies), machine2)
  })
}
