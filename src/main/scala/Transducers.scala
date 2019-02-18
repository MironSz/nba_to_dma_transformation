abstract class TransducerState[A <: Letter, B <: Letter] extends State {
  def readLetter(a: A): (B, TransducerState[A, B])

  def dmaReversedImage(dma: DMA[B],
                       startingStateInA: DeterminizedState[B]): DMA[A] =
    new DMA[A](
      DeterminizedStateWithTransducer[A, B](dma.startingState, this),
      MullerConditionImageTransducer(
        dma.condition,
        DeterminizedStateWithTransducer(dma.startingState, this))
    )
}

case class LetterToBuchiTransducer[A <: Letter](
    reachedStates: List[NondeterminizedState[A]])
    extends TransducerState[A, BuchiLetter[A]] {
  override def readLetter(
      a: A): (BuchiLetter[A], LetterToBuchiTransducer[A]) = {
    val buchiLetter = BuchiLetter[A](
      reachedStates
        .flatMap(
          state =>
            state
              .readLetter(a)
              .map(transition => (state, transition._2, transition._1)))
    )
    val newReachedStates =
      buchiLetter.reachableStates.map(letter => letter._3).distinct
    (buchiLetter, LetterToBuchiTransducer[A](newReachedStates))
  }
}

case class DagToTreeTansducer[A <: Letter](
    reachedStatesSortedByProfile: List[List[NondeterminizedState[A]]])
    extends TransducerState[BuchiLetter[A], BuchiLetter[A]] {
  override def readLetter(a: BuchiLetter[A])
    : (BuchiLetter[A], TransducerState[BuchiLetter[A], BuchiLetter[A]]) = {
    // Usunąć nieosiąglne stany
    val allReachableStates = reachedStatesSortedByProfile.flatten

    val removedUnreachableFromLetter =
      a.reachableStates.filter(b => allReachableStates contains b._1)

    val reachedStatesSortedReduced =
      reachedStatesSortedByProfile.map(l =>
        l.filter(b => removedUnreachableFromLetter.map(c => c._1) contains b))

    //mogę reachedStateSortedByProfile zzipować z
  }
}
