package aoc2023

import aoc.utils._
import aoc.utils.Math.leastCommonMultiple

class Day08(input: List[String]) {

	def newDirections = Iterator.continually(input(0)).flatten
	val graph = input.drop(2).map(_.split("[^0-9A-Z]+") match { case Array(s, l, r) => (s, (l, r)) }).toMap
	val startNodes = graph.collect { case (n, _) if n.endsWith("A") => n }.toList

	def run(start: String, isDone: String => Boolean): Long = {
		val directions = newDirections
		var node = start; var i = 0L
		while (!isDone(node)) {
			i = i + 1
			node = directions.next() match {
				case 'L' => graph(node)._1
				case 'R' => graph(node)._2
			}
		}
		i
	}

	submit(1, run("AAA", _ == "ZZZ"))
	submit(2, leastCommonMultiple(startNodes.map(start => run(start, _.endsWith("Z")))))
}
