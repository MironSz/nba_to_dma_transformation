abstract class Automaton[A<:Letter] {
	def readLetter(a:A):Any
	def acceptanceRates:List[Int]

}
