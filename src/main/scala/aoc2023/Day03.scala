package aoc2023

import aoc.utils.submit

class Day03(input: List[String]) {

	case class Sym(c: Char, coord: (Int, Int))
	case class Num(value: Int, coords: Set[(Int, Int)])

	@inline def isAdjacent(p1: (Int, Int), p2: (Int, Int)): Boolean =
		Math.abs(p1._1 - p2._1) <= 1 && Math.abs(p1._2 - p2._2) <= 1

	val positions = input.zipWithIndex.map { case (line, y) =>
		val lx = line.zipWithIndex
		val syms = lx.collect { case (c, x) if !c.isDigit && c != '.' => Sym(c, (x, y)) }
		val nums = lx.foldLeft(List.empty[Num]) {
			case (acc, (c, x)) if c.isDigit =>
				val digit = c.toInt - 48
				val curr = acc.headOption.getOrElse(Num(0, Set.empty))
				if (curr.coords.exists(_._1 == x - 1)) {
					curr.copy(value = curr.value * 10 + digit, coords = curr.coords + ((x, y))) :: acc.tail
				} else {
					Num(digit, Set((x, y))) :: acc
				}
			case (acc, _) => acc
		}
		(nums, syms)
	}

	val nums = positions.flatMap(_._1)
	val syms = positions.flatMap(_._2)
	var ratios = syms.filter(_.c == '*').map { star =>
		val adj = nums.filter(_.coords.exists(c => isAdjacent(c, star.coord)))
		if (adj.size == 2) adj.map(_.value).product else 0
	}

	submit(1, nums.filter(n => syms.exists(s => n.coords.exists(c => isAdjacent(c, s.coord)))).map(_.value).sum)
	submit(2, ratios.sum)
}
