package aoc2022

import aoc.utils.submit

class Day10(input: List[String]) {

	val width = 40

	def sample(cycle: Int, x: Int): List[Int] = Option.when((cycle - 20) % 40 == 0)(cycle * x).toList

	def draw(cycle: Int, x: Int): Char = {
		val crtx = (cycle - 1) % width
		if (x - 1 <= crtx && crtx <= x + 1) '█' else ' '
	}

	val (_, _, signalStrengths, screen) = input.foldLeft((1, 1, List.empty[Int], List.empty[Char])) {
		case ((x, cycle, signalStrengths, screen), line) =>
			line match {
				case "noop" => (x, cycle + 1, sample(cycle, x) ++ signalStrengths, draw(cycle, x) +: screen)
				case _ => (
					x + line.drop(5).toInt,
					cycle + 2,
					sample(cycle + 1, x) ++ sample(cycle, x) ++ signalStrengths,
					List(draw(cycle + 1, x), draw(cycle, x)) ++ screen
				)
			}
	}

	submit(1, signalStrengths.reverse.sum)
	submit(2, "\n" + screen.reverse.grouped(width).map(_.mkString).mkString("\n"))
}
