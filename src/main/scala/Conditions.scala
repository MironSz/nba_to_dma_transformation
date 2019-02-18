abstract class Condition[A <: Letter] {
  def evaluateCondition(a: A): Condition[A]
}

case class MullerCondition[A <: Letter](
    s: List[(List[DeterminizedState[A]], List[Int])],
    iter: Int,
    ds: DeterminizedState[A],
    prevCondition: MullerCondition[A])
    extends Condition[A] {
  def evaluateCondition(a: A): MullerCondition[A] = {
    val nextState = ds.readLetter(a)

    new MullerCondition[A](
      s.map(c =>
        if (c._1 contains nextState)
          (c._1, c._1.zip(c._2).map(x => if (x._1 == nextState) iter else x._2))
        else (c._1, c._2.map(x => -1))),
      iter + 1,
      nextState,
      this)
  }

}
case class MullerConditionImageTransducer[A <: Letter, B <: Letter](
    mc: Condition[B],
    dst: DeterminizedStateWithTransducer[A, B]
) extends Condition[A] {
  override def evaluateCondition(a: A): MullerConditionImageTransducer[A, B] = {
    val lastLetter = dst.t.readLetter(a)._1
    MullerConditionImageTransducer(mc.evaluateCondition(lastLetter),
                                   dst.readLetter(a))
  }
}
