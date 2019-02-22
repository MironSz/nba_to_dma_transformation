class DMA[A <: Letter](val startingState: DeterminizedState[A],
                       val condition: MullerCondition[DeterminizedState[A]],
                       val prevAcceptanceRates: List[Int])
    extends Automaton[A] {
  override def readLetter(a: A): DMA[A] = {
    val nextState = startingState.readLetter(a)
    new DMA[A](
      nextState,
      condition.evaluateCondition(
        new Transition[DeterminizedState[A]](startingState, nextState)
      ),
      condition.acceptanceRates :: prevAcceptanceRates
    )
  }

  override def acceptanceRates: List[Int] = prevAcceptanceRates
}

abstract class BuchiTreeDMA[A <: Letter](
    startingState: DeterminizedState[BuchiLetter[A]],
    condition: MullerCondition[DeterminizedState[BuchiLetter[A]]],
    prevAcceptanceRates: List[Int])
    extends DMA[BuchiLetter[A]](startingState, condition, prevAcceptanceRates) {}
