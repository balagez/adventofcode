package aoc2023

import aoc.utils.submit

class Day04(input: List[String]) {

	val parsed = input.zipWithIndex.map { case (line, index) =>
		val rest = line.dropWhile(_ != ':').drop(1)
		val Array(winning, have) = rest.split(" \\| ", 2).map(_.trim.split("\\s+").map(_.toInt).toList)
		val matching = have.intersect(winning).length
		((index, matching), if (matching > 0) Math.pow(2, matching.toDouble - 1).toInt else 0)
	}

	val cards = parsed.map(_._1)
	val instances = cards.foldLeft(cards.map(_._1 -> 1).toMap) { case (acc, (index, wins)) =>
		val won = (index + 1 to index + wins).toSet
		acc.map { case (i, v) => i -> (if (won.contains(i)) (v + acc(index)) else v) }
	}

	submit(1, parsed.map(_._2).sum)
	submit(2, instances.map(_._2).sum)
}
