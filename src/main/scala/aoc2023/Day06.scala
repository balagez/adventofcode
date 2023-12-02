package aoc2023

import aoc.utils._

class Day06(input: List[String]) {

	@annotation.tailrec private def rec(t: Long, d: Long, hold: Long, delta: Int): Long =
		if ((t - hold) * hold > d) hold else rec(t, d, hold + delta, delta)

	def race(t: Long, d: Long): Long = rec(t, d, hold = t - 1, delta = -1) - rec(t, d, hold = 1, delta = 1) + 1

	submit(1, input(0).longList("\\s+").zip(input(1).longList("\\s+")).map(race.tupled).product)
	submit(2, race(input(0).filter(_.isDigit).toLong, input(1).filter(_.isDigit).toLong))
}
