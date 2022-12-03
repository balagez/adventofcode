package aoc2022

import aoc.utils.submit

class Day01(input: List[String]) {

	val sums = input.foldLeft(List.empty[Int]) {
		case (Nil, num) => List(num.toInt)
		case (acc, "") => 0 +: acc
		case (head :: tail, num) => (head + num.toInt) +: tail
	}.sortBy(-_)

	submit(1, sums.head)
	submit(2, sums.take(3).sum)
}
