case class DMA[A <: Letter](startingState: DeterminizedState[A],
                            condition: Condition[A]) {
  def readLetter(a: A): DMA[A] = {
    val nextState = startingState.readLetter(a)
    new DMA[A](nextState, condition.evaluateCondition(a))
  }
}
