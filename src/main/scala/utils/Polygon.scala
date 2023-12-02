package aoc.utils

final case class Polygon(ps: Vector[Vec]) {

	lazy val (xs, ys) = (ps.map(_.x), ps.map(_.y))
	lazy val (minx, maxx, miny, maxy) = (xs.min, xs.max, ys.min, ys.max)
	lazy val boundingBox: (Vec, Vec) = (Vec(minx, miny), Vec(maxx, maxy))
	lazy val (rangex, rangey) = (minx to maxx, miny to maxy)

	lazy val loop: Vector[Vec] = ps :+ ps.head
	lazy val edges: Vector[(Vec, Vec)] = loop.zip(loop.tail)

	@inline private def isBetween(p: Int, a: Int, b: Int): Boolean = (a <= p && p <= b) || (b <= p && p <= a)

	// Even–odd rule: https://wrfranklin.org/Research/Short_Notes/pnpoly.html
	def isInside(p: Vec): Boolean = {
		val Vec(x, y) = p
		var i = 0
		var j = ps.length - 1
		var c = false
		while (i < ps.length) {
			val vi = ps(i)
			val vj = ps(j)
			if (isBetween(x, vi.x, vj.x) && isBetween(y, vi.y, vj.y)) return true
			if ((vi.y > y) != (vj.y > y) && x < (vj.x - vi.x) * (y - vi.y) / (vj.y - vi.y) + vi.x) c = !c
			j = i; i = i + 1
		}
		c
	}

	lazy val area: Long = {
		// https://en.wikipedia.org/wiki/Shoelace_formula
		val area = edges.foldLeft(0L) { case (s, (a, b)) => s + a.x.toLong * b.y - b.x.toLong * a.y } / 2

		// https://en.wikipedia.org/wiki/Pick%27s_theorem
		val boundary = edges.foldLeft(0L) { case (s, (a, b)) => s + a.distanceTo(b) }
		val inside = area - boundary / 2 + 1
		inside + boundary
	}
}
