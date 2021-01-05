package src.main.scala.chapter06

sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateCoinStep(): State[Machine, (Int, Int)] = {
    State((s: Machine) => {
      if (s.locked) ((s.coins + 1, s.candies), Machine(locked = false, s.candies, s.coins))
      else ((s.coins, s.candies), s)
    })
  }

  def simulateTurnStep(): State[Machine, (Int, Int)] = {
    State((s: Machine) => {
      if (s.locked) ((s.coins, s.candies), s)
      else ((s.coins, s.candies - 1), Machine(locked=true, candies = s.candies - 1, coins=s.coins))
    })
  }

  def simulateSingleStep(input: Input): State[Machine, (Int, Int)] = {
    State((s: Machine) => {
      if (s.candies <= 0) ((s.coins, s.candies), s)
      else {
        input match {
          case Coin => simulateCoinStep().run(s)
          case Turn => simulateTurnStep().run(s)
        }
      }
    })
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State((s: Machine) => {
      inputs.foldRight(s, (input: Input, state: State[Machine, (Int, Int)]) => {
        simulateSingleStep(input).run(state)
      })
    })
  }
}