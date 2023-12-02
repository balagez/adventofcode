package aoc2023

import aoc.utils._
import annotation.tailrec

class Day10(input: List[String]) {

	val map = input.zipWithIndex.flatMap { case (line, y) =>
		line.toList.zipWithIndex.collect { case (c, x) if c != '.' => (Vec(x, y), c) }
	}.toMap

	val start = map.collectFirst { case (p, 'S') => p }.get

	val moves = Map(
		'S' -> Vec.dirs.toSet,
		'|' -> Set(Vec.up, Vec.down),
		'-' -> Set(Vec.left, Vec.right),
		'J' -> Set(Vec.up, Vec.left),
		'F' -> Set(Vec.down, Vec.right),
		'L' -> Set(Vec.up, Vec.right),
		'7' -> Set(Vec.down, Vec.left),
	)

	val connections = Map(
		Vec.up -> Set('S', '|', '7', 'F'),
		Vec.right -> Set('S', '-', 'J', '7'),
		Vec.down -> Set('S', '|', 'J', 'L'),
		Vec.left -> Set('S', '-', 'L', 'F'),
	)

	def directionsFrom(p: Vec): Set[Vec] = moves(map(p)).flatMap { move =>
		val nextp = p + move
		map.get(nextp).collect { case nextc if connections(move).contains(nextc) => move }
	}

	def nextFrom(p: Vec): Set[Vec] = directionsFrom(p).map(p + _)

	@tailrec private def run(p: Vec, trail: List[Vec]): List[Vec] = map(p) match {
		case 'S' if trail.nonEmpty => trail
		case 'S' => run(nextFrom(p).head, p :: trail) // choose any direction
		case _ => run(nextFrom(p).filterNot(_ == trail.head).head, p :: trail)
	}

	val trail = run(start, List()).toSet
	val mapTrailOnly = map.collect { // keep the map only for the trail, replace S with actual edge
		case (p, 'S') => (p, moves.collectFirst { case (c, ds) if ds == directionsFrom(p) => c }.getOrElse('S'))
		case (p, c) if trail.contains(p) => (p, c)
	}
	val (xs, ys) = (trail.map(_.x), trail.map(_.y))
	val (minx, maxx, miny, maxy) = (xs.min, xs.max, ys.min, ys.max)
	val (changes, nochanges) = ("(F-*J)|(L-*7)".r, "(F-*7)|(L-*J)".r) // edge patterns that don't change in/out status

	private var isInside = false
	private var inside = 0
	(miny to maxy).foreach { y =>
		val line = (minx to maxx).map(x => mapTrailOnly.getOrElse(Vec(x, y), '.')).mkString
		changes.replaceAllIn(nochanges.replaceAllIn(line, ""), "|").foreach { // collapse edge patterns
			case '|' => isInside = !isInside
			case '.' => if (isInside) inside = inside + 1
			case _ => ()
		}
	}

	submit(1, trail.size / 2)
	submit(2, inside)
}
