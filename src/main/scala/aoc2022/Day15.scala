package aoc2022

import aoc.utils._
import scala.collection.parallel.CollectionConverters._

class Day15(input: List[String]) {

	case class Sensor(sx: Int, sy: Int, bx: Int, by: Int) {

		val radius = math.abs(sx - bx) + math.abs(sy - by)
		val top = sy - radius
		val bottom = sy + radius

		@inline def intersect(y: Int): Option[Interval[Int]] = Option.when(top <= y && y <= bottom) {
			val delta = radius - math.abs(sy - y)
			Interval(sx - delta, sx + delta)
		}
	}

	val Line = ".*x=([^,]+), y=([^:]+).*x=([^,]+), y=(.*)$".r
	val sensors = input.map { case Line(sx, sy, bx, by) => Sensor(sx.toInt, sy.toInt, bx.toInt, by.toInt) }
	val isExample = sensors.length == 14

	def intersect(y: Int): Seq[Interval[Int]] = Interval.merge(sensors.flatMap(_.intersect(y)))

	val y = (0 to (if (isExample) 20 else 4000000)).par.find(y => intersect(y).length > 1).get
	val x = BigInt(intersect(y)(0).end + 1)

	submit(1, intersect(if (isExample) 10 else 2000000).head.length - 1)
	submit(2, x * 4000000 + y)
}
