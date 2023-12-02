package aoc2023

import aoc.utils._
import scala.annotation.tailrec

class Day23(input: List[String]) extends Show {

	case class Path(head: Vec, steps: Int, nodes: Set[Vec])

	val PATH = '█'

	val map = input.zipWithIndex.flatMap { case (line, y) =>
		line.zipWithIndex.collect { case (c, x) if c != '#' => Vec(x, y) -> (if (c == '.') PATH else c) }
	}.toMap

	val (start, end) = (Vec(1, 0), Vec(input.head.length - 2, input.length - 1))

	def dirsWithSlopes(current: Vec): List[Vec] = map(current) match {
		case 'v' => List(Vec.down)
		case '>' => List(Vec.right)
		case _ => Vec.dirs
	}

	def search(dirsFrom: Vec => List[Vec]): Int = {
		@tailrec def loop(active: List[Path], finished: List[Path], visited: Set[Vec]): Int =
			if (active.isEmpty /*|| i == 0*/ ) {
				val res = finished.filter(_.head == end)
				// res.foreach(p => println((p.steps, p.nodes.size)))
				res.map(_.steps).max
			} else {
				val newPaths = active.map {
					case path @ Path(`end`, _, _) => Left(path)
					case path @ Path(current, steps, nodes) =>
						val next = dirsFrom(current).map(current + _).filter(p => map.contains(p) && !path.nodes(p)).toList
						if (next.isEmpty) Left(path)
						else Right(next.map(n => Path(n, steps + 1, nodes + n)))
				}
				val newActive = newPaths.collect { case Right(xs) => xs }.flatten
				loop(
					active = newActive,
					finished = finished ++ newPaths.collect { case Left(xs) => xs },
					visited = visited ++ newActive.map(_.head)
				)
			}
		loop(List(Path(start, 0, Set(start))), List(), Set(start))
	}

	submit(1, search(dirsWithSlopes))
	// submit(2, search(_ => Vec.dirs))
}
