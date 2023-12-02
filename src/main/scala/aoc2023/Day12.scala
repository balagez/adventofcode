package aoc2023

import aoc.utils._
import collection.mutable.HashMap

// based on https://github.com/dhconnelly/advent-of-code-2023/blob/master/src/day12.rs
class Day12(input: List[String]) {

	val in1 = input.map { line =>
		val Array(springs, spans) = line.split(" ", 2)
		(springs, spans.intList(","))
	}

	val in2 = in1.map { case (springs, spans) => (List.fill(5)(springs).mkString("?"), List.fill(5)(spans).flatten) }

	sealed trait Outcome { def value: Long }
	case object Invalid extends Outcome { def value = 0 }
	case class Valid(value: Long) extends Outcome

	def place(len: Int, springs: List[Char], lens: List[Int], cache: HashMap[(Int, Int), Outcome]): Outcome =
		if (len > springs.length) {
			Invalid
		} else if (springs.take(len).contains('.')) {
			Invalid
		} else if (len >= springs.length) {
			arrangements(springs.drop(len), lens, cache)
		} else if (springs(len) == '#') {
			Invalid
		} else {
			arrangements(springs.drop(len + 1), lens, cache)
		}

	def arrangements(springs: List[Char], lens: List[Int], cache: HashMap[(Int, Int), Outcome]): Outcome = {
		val key = (springs.length, lens.length)
		cache.getOrElse(key, {
			val outcome: Outcome = (springs.headOption, lens.headOption) match {
				case (None, None) => Valid(1)
				case (None, Some(_)) => Invalid
				case (Some('.'), _) => arrangements(springs.tail, lens, cache)
				case (Some('#'), None) => Invalid
				case (Some('#'), Some(len)) => place(len, springs, lens.tail, cache)
				case (Some('?'), None) => arrangements(springs.tail, lens, cache)
				case (Some('?'), Some(len)) =>
					val here = place(len, springs, lens.tail, cache).value
					val there = arrangements(springs.tail, lens, cache).value
					Valid(here + there)
				case _ => Invalid
			}
			cache.update(key, outcome)
			outcome
		})
	}

	def run(springs: String, lens: List[Int]): Long =
		arrangements(springs.toList, lens, HashMap.empty[(Int, Int), Outcome]).value

	submit(1, in1.map(run.tupled).sum)
	submit(2, in2.map(run.tupled).sum)
}
