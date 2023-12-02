package aoc2023

import aoc.utils._

class Day14(input: List[String]) {

	type M = collection.mutable.Map[Vec, Char]

	val map = input.zipWithIndex.flatMap { case (line, y) =>
		line.toList.zipWithIndex.collect { case (c, x) if Set('#', 'O').contains(c) => (Vec(x, y), c) }
	}.toMap

	val (w, h) = (input.head.length, input.length)

	sealed trait Tilt {
		val iterate: (Vec => Unit) => Unit // iterate over the map in the tilt direction
		val move: (M, Vec) => Vec // move the point in the tilt direction

		def ifShouldRoll(m: M, p: Vec)(block: => Unit): Unit = m.get(p).filter(_ == 'O').foreach(_ => block)

		def apply(acc: M): Unit = iterate(p =>
			ifShouldRoll(acc, p) {
				val moved = move(acc, p)
				if (moved != p) { acc.remove(p); acc.update(moved, 'O') }
			}
		)
	}
	case object North extends Tilt {
		val iterate = block => (0 to h - 1).foreach(y => (0 until w).foreach(x => block(Vec(x, y))))
		val move = (m, p) => p.copy(y = (p.y - 1 to 0 by -1).find(yy => m.contains(Vec(p.x, yy))).getOrElse(-1) + 1)
	}
	case object South extends Tilt {
		val iterate = block => (h - 1 to 0 by -1).foreach(y => (0 until w).foreach(x => block(Vec(x, y))))
		val move = (m, p) => p.copy(y = (p.y + 1 to h - 1).find(yy => m.contains(Vec(p.x, yy))).getOrElse(h) - 1)
	}
	case object West extends Tilt {
		val iterate = block => (0 to w - 1).foreach(x => (0 until h).foreach(y => block(Vec(x, y))))
		val move = (m, p) => p.copy(x = (p.x - 1 to 0 by -1).find(xx => m.contains(Vec(xx, p.y))).getOrElse(-1) + 1)
	}
	case object East extends Tilt {
		val iterate = block => (w - 1 to 0 by -1).foreach(x => (0 until h).foreach(y => block(Vec(x, y))))
		val move = (m, p) => p.copy(x = (p.x + 1 to w - 1).find(xx => m.contains(Vec(xx, p.y))).getOrElse(w) - 1)
	}

	def run(count: Int): Map[Vec, Char] = {
		val m = map.to(collection.mutable.Map)
		val cycle = collection.mutable.ListBuffer.empty[Map[Vec, Char]]
		val hashCodes = collection.mutable.Set.empty[Int]
		var cycleFound = false
		do {
			List(North, West, South, East).foreach(_.apply(m))
			val current = m.to(Map)
			if (hashCodes.contains(current.hashCode)) cycleFound = true
			cycle.append(current)
			hashCodes += current.hashCode
		} while (!cycleFound)
		val start = cycle.indexOf(cycle.last)
		cycle(start - 1 + (count - start) % (cycle.length - start - 1))
	}

	def weight(m: collection.Map[Vec, Char]): Int = m.collect { case (Vec(_, y), 'O') => h - y }.sum

	submit(1, weight({ val m = map.to(collection.mutable.Map); North.apply(m); m }))
	submit(2, weight(run(1000000000)))
}
