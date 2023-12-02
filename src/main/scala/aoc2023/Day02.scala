package aoc2023

import aoc.utils.submit

class Day02(input: List[String]) {

	val target = Map("red" -> 12, "green" -> 13, "blue" -> 14)

	val games = input.map { line =>
		val Array(head, rest) = line.split(": ", 2)
		head.drop(5).toInt -> rest.split("; ").toList.map(
			_.split(", ").toList.map(_.split(" ") match { case Array(num, color) => (num.toInt, color) })
		)
	}

	submit(1, games.filter(_._2.forall(_.forall(draw => draw._1 <= target.getOrElse(draw._2, 0)))).map(_._1).sum)
	submit(2, games.map(_._2.flatten.groupMap(_._2)(_._1).map(_._2.max).product).sum)
}
