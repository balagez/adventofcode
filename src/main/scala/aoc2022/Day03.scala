package aoc2022

import aoc.utils.submit

class Day03(input: List[String]) {

	@inline def priority(c: Char): Int = if (c.isLower) c.toInt - 96 else c.toInt - 38

	val common = input.map { line =>
		val (left, right) = line.splitAt(line.length / 2)
		priority(left.intersect(right).head)
	}.sum

	val badges = input.grouped(3).map(group => priority(group.reduce(_ intersect _).head)).sum

	submit(1, common)
	submit(2, badges)
}
