package aoc2022

import aoc.utils._

/**
 * Based on https://github.com/betaveros/advent-of-code-2022/blob/main/p10.noul
 */
class Day10a(input: List[String]) {

	val values = input.map(words).flatMap {
		case "addx" :: n :: _ => List(0, n.toInt)
		case _ => List(0)
	}.scan(1)(_ + _)

	val crt = values.grouped(40)
		.map(_.zipWithIndex.map { case (x, i) => if (math.abs(x - i) <= 1) '█' else ' ' }.mkString)
		.mkString("\n")

	submit(1, (20 to 220 by 40).map(x => x * values(x - 1)).sum)
	submit(2, "\n" + crt)
}
