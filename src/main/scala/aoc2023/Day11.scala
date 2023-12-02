package aoc2023

import aoc.utils._
import math.{ min, max }

class Day11(input: List[String]) {

	val map = input.zipWithIndex.flatMap { case (line, y) =>
		line.toList.zipWithIndex.collect { case (c, x) if c == '#' => Vec(x, y) }
	}

	val (xs, ys) = (map.map(_.x), map.map(_.y))
	val (rangex, rangey) = (xs.min to xs.max, ys.min to ys.max)
	val (expx, expy) = (rangex.filterNot(xs.contains).toSet, rangey.filterNot(ys.contains).toSet)

	def dist(expansion: Int)(a: Vec, b: Vec): Long = {
		val (rangex, rangey) = (min(a.x, b.x) to max(a.x, b.x), min(a.y, b.y) to max(a.y, b.y))
		val dx = rangex.foldLeft(0L)((acc, x) => acc + (if (expx.contains(x)) expansion else 1)) - 1
		val dy = rangey.foldLeft(0L)((acc, y) => acc + (if (expy.contains(y)) expansion else 1)) - 1
		dx + dy
	}

	val pairs = map.zipWithIndex.flatMap { case (a, i) => map.drop(i + 1).map((a, _)) } // don't duplicate pairs

	submit(1, pairs.map(dist(2).tupled).sum)
	submit(2, pairs.map(dist(1000000).tupled).sum)
}
