package aoc.utils

import scala.annotation.tailrec
import scala.math.abs

object Math {

	// Euclidean algorithm
	@tailrec def greatestCommonDivisor(a: Long, b: Long): Long = if (b == 0) a else greatestCommonDivisor(b, a % b)

	def leastCommonMultiple(a: Long, b: Long): Long = abs(a * b) / greatestCommonDivisor(a, b)

	def leastCommonMultiple(ns: Seq[Long]): Long = ns.reduce(leastCommonMultiple)
}
