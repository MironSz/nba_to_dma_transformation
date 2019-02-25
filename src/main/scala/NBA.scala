class NBA[A <: Letter](currentStates: List[(NondeterminizedState[A], Int)],
                       prevConditions: List[Int])
    extends Automaton[A] {
  lazy val allStates: List[NondeterminizedState[A]] = null

  override def readLetter(a: A): NBA[A] = {
    print(currentStates+"\n")
    val allReachable = currentStates.map(
      state =>
        state._1
          .readLetter(a)
          .map(nextState =>
            (nextState._1, (if (nextState._2) 1 else 0) + state._2)))
//    print(allReachable)
    val reachableWithoutDuplicates = allReachable.flatten
      .groupBy(_._1)
      .toList
      .map(c => (c._1, c._2.map(_._2)))
      .map(c => (c._1, c._2.max))
//    print(reachableWithoutDuplicates)
    val newPrevConditions: List[Int] = try {
      reachableWithoutDuplicates.map(b => b._2).max :: prevConditions
    } catch { case _: Throwable => 0 :: prevConditions }
    new NBA[A](reachableWithoutDuplicates, newPrevConditions)
  }

  override def acceptanceRates: List[Int] = prevConditions

}
