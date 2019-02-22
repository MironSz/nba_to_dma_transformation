abstract class DeterminizedState[A <: Letter] extends State {
  def readLetter(a: A): DeterminizedState[A]
}

class MultipliedDeterminizedState[A <: Letter, B <: Letter](
    s1: DeterminizedState[A],
    s2: DeterminizedState[B])
    extends DeterminizedState[MultipliedLetter[A, B]] {
  def readLetter(
      ab: MultipliedLetter[A, B]): MultipliedDeterminizedState[A, B] =
    new MultipliedDeterminizedState[A, B](s1.readLetter(ab.a),
                                          s2.readLetter(ab.b))
}

class DeterminizedStateWithTransducer[A <: Letter, B <: Letter](
    val s: DeterminizedState[B],
    val t: TransducerState[A, B])
    extends DeterminizedState[A] {
  override def readLetter(a: A): DeterminizedStateWithTransducer[A, B] = {
    val t2 = t.readLetter(a)
    new DeterminizedStateWithTransducer[A, B](s.readLetter(t2._1),
                                              t.readLetter(a)._2)
  }
}
