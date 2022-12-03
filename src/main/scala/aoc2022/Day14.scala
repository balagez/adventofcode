package aoc2022

import aoc.utils.submit
import scala.collection.mutable

class Day14(input: List[String]) {

	val paths = input.map(_.split(" -> ").toList.map { xy => val Array(x, y) = xy.split(",").map(_.toInt); (x, y) })
	val start = (500, 0)
	val maxy = paths.flatMap(_.map(_._2)).max

	case class Map(height: Int) {

		private val map = mutable.Map.empty[(Int, Int), Char]

		@inline def free(p: (Int, Int)): Boolean = p._2 <= height && !map.contains(p)
		@inline def update(p: (Int, Int), c: Char): Unit = map.update(p, c)

		for { path <- paths; List((x1, y1), (x2, y2)) <- path.sliding(2) } yield {
			if (y1 == y2) (x1 to x2 by math.signum(x2 - x1)).foreach(x => update((x, y1), '#'))
			if (x1 == x2) (y1 to y2 by math.signum(y2 - y1)).foreach(y => update((x1, y), '#'))
		}
	}

	def run(height: Int, isFull: (Int, Map) => Boolean): Int = {
		val map = Map(height)
		var full = false
		var i = 0
		while (!full) {
			var sand = start
			var resting = false
			while (!resting) {
				val (cx, cy) = sand
				LazyList((cx, cy + 1), (cx - 1, cy + 1), (cx + 1, cy + 1)).find(map.free) match {
					case Some(next) => sand = next
					case None => resting = true
				}
			}
			map.update(sand, 'o')
			i = i + 1
			full = isFull(sand._2, map)
		}
		i
	}

	submit(1, run(maxy, { case (sandy, map) => sandy >= map.height }) - 1)
	submit(2, run(maxy + 1, { case (_, map) => !map.free(start) }))
}
