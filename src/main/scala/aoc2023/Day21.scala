package aoc2023

import aoc.utils._
import scala.annotation.tailrec

class Day21(input: List[String]) {

	val map = input.zipWithIndex.flatMap { case (line, y) =>
		line.zipWithIndex.collect {
			case (c, x) if c == '#' => Left(Vec(x, y))
			case (c, x) if c == 'S' => Right(Vec(x, y))
		}
	}.toSet

	val wh = Vec(input.head.length, input.length)

	val start = map.collectFirst({ case Right(p) => p }).get
	val rocks = map.collect({ case Left(p) => p })

	def bfs(start: Vec, steps: Int): Int = {
		val visited = collection.mutable.Set.empty[Vec]
		@tailrec def loop(current: Set[Vec], remaining: Int): Int =
			if (remaining == 0) {
				// One step changes one coordinate by one so if the number of steps is even
				// then the sum of coordinates is even. All visited points with this property
				// are reachable in the given number of steps because we may be just going
				// back and forth to stay in closer ones.
				visited.filter(p => (p.x + p.y) % 2 == steps % 2).size
			} else {
				val next = current.flatMap(p =>
					Vec.dirs.map(p + _).filterNot(rocks.contains).filterNot(visited.contains)
				)
				visited ++= next
				loop(next, remaining - 1)
			}
		loop(Set(start), steps)
	}

	submit(1, bfs(start, 64))
	submit(2, 0)
}
