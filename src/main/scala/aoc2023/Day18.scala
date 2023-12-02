package aoc2023

import aoc.utils._
import aoc.utils.Vec.{ up, down, left, right }
import scala.annotation.tailrec

class Day18(input: List[String]) {

	val plan1 = input.map { line =>
		val d1 :: n :: _ = line.split(" ").toList
		Map('R' -> right, 'D' -> down, 'L' -> left, 'U' -> up)(d1.head) -> n.toInt
	}
	val plan2 = input.map { line =>
		val (hex, d2) = line.dropWhile(_ != '#').drop(1).splitAt(5)
		Map('0' -> right, '1' -> down, '2' -> left, '3' -> up)(d2.head) -> Integer.parseInt(hex, 16)
	}

	def run(plan: List[(Vec, Int)]): Long = {
		@tailrec def loop(pos: Vec, plan: List[(Vec, Int)], acc: Vector[Vec]): Vector[Vec] = plan match {
			case Nil => acc.reverse
			case (dir, dist) :: tail => loop(pos + dir * dist, tail, pos +: acc)
		}
		Polygon(loop(Vec.zero, plan, Vector())).area
	}

	submit(1, run(plan1))
	submit(2, run(plan2))
}
