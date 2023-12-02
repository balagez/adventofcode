package aoc2022

import aoc.utils._

class Day09(input: List[String]) {

	case class Rope(knots: List[Vec], trail: Set[Vec]) {
		def runWith(steps: List[Vec]): Rope = steps.foldLeft(this) { (acc, step) =>
			val newknots = acc.knots.foldLeft(List.empty[Vec]) {
				case (Nil, curr) => List(curr + step)
				case (acc @ (prev :: _), curr) => (if (curr.isNextTo(prev)) curr else curr + (prev - curr).signum) +: acc
			}
			Rope(newknots.reverse, acc.trail + newknots.head)
		}
	}

	def rope(n: Int): Rope = Rope(List.fill(n)(Vec(0, 0)), Set(Vec(0, 0)))

	val steps = input.flatMap { line =>
		val Array(step, amount) = line.split(" ")
		List.fill(amount.toInt)(step match {
			case "R" => Vec(1, 0)
			case "L" => Vec(-1, 0)
			case "U" => Vec(0, 1)
			case "D" => Vec(0, -1)
			case _ => Vec(0, 0)
		})
	}

	submit(1, rope(2).runWith(steps).trail.size)
	submit(2, rope(10).runWith(steps).trail.size)
}
