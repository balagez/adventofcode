package aoc2022

import aoc.utils._
import collection.mutable

class Day12(input: List[String]) {

	val width = input.head.size
	val height = input.size
	var start = (0, 0)
	var end = (0, 0)

	val map = input.toVector.zipWithIndex.map {
		case (line, y) => line.toVector.zipWithIndex.map {
			case ('S', x) => { start = (x, y); 0 }
			case ('E', x) => { end = (x, y); 25 }
			case (c, _) => c.toInt - 97
		}
	}.transpose

	def adjacent(x: Int, y: Int): Seq[(Int, Int)] =
		List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter {
			case (nx, ny) => nx >= 0 && ny >= 0 && nx < width && ny < height && map(nx)(ny) >= map(x)(y) - 1
		}

	def search(done: ((Int, Int)) => Boolean): Int = {
		val explored = mutable.Set(end)
		var todo = mutable.Set(end)
		var length = 0
		while (todo.nonEmpty) {
			val next = mutable.Set.empty[(Int, Int)]
			for {
				p <- todo
				neighbor <- adjacent.tupled(p).filterNot(explored.contains)
			} yield {
				if (done(neighbor)) return length + 1
				explored += p
				next += neighbor
			}
			todo = next
			length = length + 1
		}
		length
	}

	submit(1, search(_ == start))
	submit(2, search { case (x, y) => map(x)(y) == 0 })
}
