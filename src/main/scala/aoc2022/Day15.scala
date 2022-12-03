package aoc2022

import aoc.utils._
import scala.collection.parallel.CollectionConverters._

class Day15(input: List[String]) {

	type Interval = (Int, Int)

	case class Sensor(sx: Int, sy: Int, bx: Int, by: Int) {

		val radius: Int = math.abs(sx - bx) + math.abs(sy - by)
		val top: Int = sy - radius
		val bottom: Int = sy + radius

		@inline def intersect(y: Int): Option[Interval] = Option.when(top <= y && y <= bottom) {
			val delta = radius - math.abs(sy - y)
			(sx - delta, sx + delta)
		}
	}

	val Line = ".*x=([^,]+), y=([^:]+).*x=([^,]+), y=(.*)$".r
	val sensors = input.map { case Line(sx, sy, bx, by) => Sensor(sx.toInt, sy.toInt, bx.toInt, by.toInt) }
	val isExample = sensors.length == 14

	def add(i1: Interval, i2: Interval): List[Interval] = {
		val (s1, e1) = i1
		val (s2, e2) = i2
		if (e2 < s1 - 1 || e1 + 1 < s2) List(i1, i2)
		else List((math.min(s1, s2), math.max(e1, e2)))
	}

	def merge(intervals: List[Interval]): List[Interval] = {
		@annotation.tailrec def rec(is: List[Interval], acc: List[Interval]): List[Interval] =
			if (is.isEmpty) acc
			else if (acc.isEmpty) rec(is.tail, List(is.head))
			else rec(is.tail, add(is.head, acc.head) ++ acc.tail)
		rec(intervals.sorted, List()).sorted
	}

	def intersect(y: Int): List[Interval] = merge(sensors.flatMap(_.intersect(y)))

	def length(interval: Interval): Int = math.abs(interval._2 - interval._1)

	val y = (0 to (if (isExample) 20 else 4000000)).par.find(y => intersect(y).length > 1).get
	val x = BigInt(intersect(y)(0)._2 + 1)

	submit(1, length(intersect(if (isExample) 10 else 2000000).head))
	submit(2, x * 4000000 + y)
}
