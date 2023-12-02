package aoc2023

import aoc.utils._
import aoc.utils.Graph.dijkstra

class Day17(input: List[String]) {

	case class Trail(p: Vec, dir: Vec, length: Int)

	val map = input.map(_.toVector.map(_.toInt - 48)).toVector
	val (w, h) = (input.head.length, input.length)

	def successorsFrom(min: Int, max: Int)(trail: Trail): List[(Trail, Int)] =
		for {
			dir <- if (trail.length < min) List(trail.dir) else Vec.dirs
			nextp = trail.p + dir
			if !(nextp.x < 0 || nextp.x >= w || nextp.y < 0 || nextp.y >= h)
			nextTrail <- if (dir != trail.dir * -1 && dir != trail.dir) {
				List(Trail(nextp, dir, 1))
			} else if (trail.length < max && dir == trail.dir) {
				List(Trail(nextp, dir, trail.length + 1))
			} else {
				List()
			}
		} yield (nextTrail, map(nextp.y)(nextp.x))

	val target = Vec(w - 1, h - 1)

	submit(1, dijkstra[Trail](Trail(Vec.zero, Vec.right, 0), successorsFrom(min = 0, max = 3), _.p == target))
	submit(2, dijkstra[Trail](Trail(Vec.zero, Vec.down, 0), successorsFrom(min = 4, max = 10), _.p == target))
}
