package aoc2022

import aoc.utils._

class Day02(input: List[String]) {

	val toIndex = Map("A" -> 0, "B" -> 1, "C" -> 2, "X" -> 0, "Y" -> 1, "Z" -> 2)

	val indices = input.map(words).flatMap {
		case left :: right :: _ => List(toIndex(left) -> toIndex(right))
		case _ => Nil
	}

	@inline def scoreForShape(index: Int): Int = index + 1

	val scoreForOutcome = Vector(
		Vector(3, 6, 0),
		Vector(0, 3, 6),
		Vector(6, 0, 3)
	)

	val strategy1 = indices.map { case (opponent, me) => scoreForShape(me) + scoreForOutcome(opponent)(me) }

	val strategy2 = indices.map {
		case (opponent, outcome) =>
			val me = scoreForOutcome(opponent).indexOf(outcome * 3)
			scoreForShape(me) + scoreForOutcome(opponent)(me)
	}

	submit(1, strategy1.sum)
	submit(2, strategy2.sum)
}
