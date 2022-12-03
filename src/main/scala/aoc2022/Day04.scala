package aoc2022

import aoc.utils.submit

class Day04(input: List[String]) {

	val (contains, overlaps) = input.foldLeft((0, 0)) {
		case ((contains, overlaps), line) =>
			val Array(Array(s1, e1), Array(s2, e2)) = line.split(",").map(_.split("-").map(_.toInt))
			(
				if ((s1 >= s2 && e1 <= e2) || (s2 >= s1 && e2 <= e1)) contains + 1 else contains,
				if (!(e1 < s2 || e2 < s1 || s1 > e2 || s2 > e1)) overlaps + 1 else overlaps
			)
	}

	submit(1, contains)
	submit(2, overlaps)
}
