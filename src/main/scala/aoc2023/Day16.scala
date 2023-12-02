package aoc2023

import aoc.utils._

class Day16(input: List[String]) {

	import Vec.{ left, right, up, down }

	case class Beam(p: Vec, dir: Vec) { @inline def pointTo(dir: Vec): Beam = copy(dir = dir) }
	case class Tile(c: Char, energized: Boolean = false, visitedFrom: Set[Vec] = Set())

	val (w, h) = (input.head.length, input.length)
	val initialMap = input.zipWithIndex.flatMap { case (l, y) =>
		l.zipWithIndex.map { case (c, x) => (Vec(x, y), Tile(c)) }
	}.toMap

	@inline def isOutside(p: Vec): Boolean = p.x < 0 || p.x >= w || p.y < 0 || p.y >= h

	val reflectfw = Map(up -> right, right -> up, down -> left, left -> down)
	val reflectbw = Map(up -> left, left -> up, down -> right, right -> down)

	def run(start: Beam): Int = {
		var beams = List(start)
		val map = initialMap.to(collection.mutable.Map) // make a mutable copy

		while (beams.nonEmpty) {
			beams = beams.flatMap { curr =>
				val next = Beam(curr.p + curr.dir, curr.dir) // advance beam in its current direction

				if (!isOutside(curr.p)) map.update(curr.p, map(curr.p).copy(energized = true))

				if (isOutside(next.p)) {
					List() // beam hit the wall
				} else {
					val tile = map(next.p)
					map.update(next.p, tile.copy(visitedFrom = map(next.p).visitedFrom + curr.p))
					tile.c match {
						// optimization: we have already seen this beam, don't propagate any further
						case _ if tile.visitedFrom.contains(curr.p) => List()
						// pass through
						case '.' => List(next)
						case '|' if curr.dir == up || curr.dir == down => List(next)
						case '-' if curr.dir == left || curr.dir == right => List(next)
						// split
						case '|' => List(up, down).map(next.pointTo)
						case '-' => List(left, right).map(next.pointTo)
						// reflect
						case '/' => List(next.pointTo(reflectfw(curr.dir)))
						case '\\' => List(next.pointTo(reflectbw(curr.dir)))
						// n/a
						case _ => List()
					}
				}
			}
		}
		map.count(_._2.energized)
	}

	val start = (0 until w).flatMap(x => List(Beam(Vec(x, -1), down), Beam(Vec(x, h), up))) ++
		(0 until h).flatMap(y => List(Beam(Vec(-1, y), right), Beam(Vec(w, y), left)))

	submit(1, run(Beam(Vec(-1, 0), right)))
	submit(2, start.map(run).max)
}
