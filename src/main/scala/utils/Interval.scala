package aoc.utils

import scala.annotation.tailrec

final case class Interval[T](start: T, end: T)(implicit ev: Integral[T]) {

	override def toString: String = s"$start..$end"

	lazy val isEmpty: Boolean = ev.gt(start, end)

	lazy val nonEmpty: Boolean = ev.lteq(start, end)

	lazy val length: T = ev.plus(ev.minus(end, start), ev.one)

	def contains(key: T): Boolean = ev.lteq(start, key) && ev.lteq(key, end)

	def +(that: Interval[T]): List[Interval[T]] =
		if (ev.lt(that.end, ev.minus(start, ev.one)) || ev.lt(ev.plus(end, ev.one), that.start)) List(this, that)
		else List(Interval(ev.min(start, that.start), ev.max(end, that.end)))

	def -(that: Interval[T]): List[Interval[T]] =
		if (ev.lt(that.end, start) || ev.lt(end, that.start)) List(this)
		else if (ev.lteq(that.start, start) && ev.lteq(end, that.end)) Nil
		else if (ev.lteq(that.start, start)) List(Interval(ev.plus(that.end, ev.one), end))
		else if (ev.lteq(end, that.end)) List(Interval(start, ev.minus(that.start, ev.one)))
		else List(Interval(start, ev.minus(that.start, ev.one)), Interval(ev.plus(that.end, ev.one), end))

	def intersect(that: Interval[T]): List[Interval[T]] =
		if (ev.lt(that.end, start) || ev.lt(end, that.start)) Nil
		else List(Interval(ev.max(start, that.start), ev.min(end, that.end)))
}

object Interval {

	implicit def ordering[T: Ordering]: Ordering[Interval[T]] = Ordering.Tuple2[T, T].on(r => (r.start, r.end))

	def merge[T: Ordering](ranges: Seq[Interval[T]]): Seq[Interval[T]] = {
		@tailrec def rec(is: Seq[Interval[T]], acc: Seq[Interval[T]]): Seq[Interval[T]] =
			if (is.isEmpty) acc
			else if (acc.isEmpty) rec(is.tail, List(is.head))
			else rec(is.tail, (is.head + acc.head) ++ acc.tail)
		rec(ranges.sorted, List()).sorted
	}
}
