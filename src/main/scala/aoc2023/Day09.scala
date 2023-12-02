package aoc2023

import aoc.utils._
import annotation.tailrec

class Day09(input: List[String]) {

	val histories = input.map(_.split(" ").map(_.toLong).toList)

	@tailrec private def diff(xs: List[Long], acc: List[Long] = List.empty): List[Long] =
		if (xs.forall(_ == 0)) acc else diff(xs.sliding(2).map(p => p(1) - p(0)).toList, xs.last +: acc)

	def extrapolate(xs: List[Long]): Long = xs.foldLeft(0L)(_ + _)

	submit(1, histories.map(nums => extrapolate(diff(nums))).sum)
	submit(2, histories.map(nums => extrapolate(diff(nums.reverse))).sum)
}
