package aoc2023

import aoc.utils._

class Day13(input: List[String]) {

	sealed trait Mirror {
		def at: Int
		def value: Int = at
	}
	case object NoMirror extends Mirror { val at = -1 }
	case class Horizontal(at: Int) extends Mirror { override def value: Int = at * 100 }
	case class Vertical(at: Int) extends Mirror

	case class Pattern(data: Vector[Vector[Char]], w: Int, h: Int) {

		def transposed: Pattern = copy(data = data.transpose, w = h, h = w)

		def isMirror(at: Int): Boolean = !(0 to math.min(at, h - at) - 1).exists { d => data(at - 1 - d) != data(at + d) }

		def findMirror(skip: Int): Option[Int] = (1 to h - 1).to(LazyList).filterNot(_ == skip).find(isMirror)

		def findMirror(skip: Mirror): Mirror = {
			val skipHorizontal = skip match { case Horizontal(at) => at; case _ => NoMirror.at }
			val skipVertical = skip match { case Vertical(at) => at; case _ => NoMirror.at }
			findMirror(skipHorizontal).map(Horizontal(_))
				.orElse(transposed.findMirror(skipVertical).map(Vertical(_)))
				.getOrElse(NoMirror)
		}

		lazy val original: Mirror = findMirror(skip = NoMirror)

		lazy val cleaned: Mirror = (for {
			y <- (0 to h - 1).to(LazyList)
			x <- (0 to w - 1).to(LazyList)
			flipped = copy(data = data.updated(y, data(y).updated(x, if (data(y)(x) == '#') '.' else '#')))
			res = flipped.findMirror(skip = original)
			if res != NoMirror
		} yield res).head
	}

	val patterns = input.foldLeft(List.empty[Vector[Vector[Char]]])((acc, line) =>
		if (line.isEmpty) acc :+ Vector.empty
		else if (acc.isEmpty) acc :+ Vector(line.toVector)
		else acc.init :+ (acc.last :+ line.toVector)
	).map(data => Pattern(data, w = data(0).length, h = data.length))

	submit(1, patterns.map(_.original.value).sum)
	submit(2, patterns.map(_.cleaned.value).sum)
}
