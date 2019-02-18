class DMA[A <: Letter](val startingState: DeterminizedState[A],
                       val condition: Condition[A]) {
  def readLetter(a: A): DMA[A] = {
    val nextState = startingState.readLetter(a)
    new DMA[A](nextState, condition.evaluateCondition(a))
  }
}


