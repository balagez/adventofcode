package aoc2023

import aoc.utils.submit
import annotation.tailrec

class Day01(input: List[String]) {

	val mapping1 = (1 to 9).map(i => (i.toString, i)).toList
	val mapping2 = List(
		"one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5,
		"six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
	)

	@tailrec private def search(mapping: List[(String, Int)], s: String): Int =
		if (s.isEmpty) 0
		else mapping.find { case (needle, _) => s.startsWith(needle) } match {
			case Some((_, value)) => value
			case None => search(mapping, s.drop(1))
		}

	def sum(mapping: List[(String, Int)]): Int = {
		val revmapping = mapping.map { case (needle, value) => (needle.reverse, value) }
		input.foldLeft(0)((acc, line) => acc + search(mapping, line) * 10 + search(revmapping, line.reverse))
	}

	submit(1, sum(mapping1))
	submit(2, sum(mapping1 ++ mapping2))
}
