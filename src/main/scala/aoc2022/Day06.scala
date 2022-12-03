package aoc2022

import aoc.utils.submit

class Day06(input: List[String]) {

	def find(in: String, window: Int): Int = in.indexOf(in.sliding(window).find(_.distinct.length == window).head) + window

	submit(1, find(input.head, 4))
	submit(2, find(input.head, 14))
}
