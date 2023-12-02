package aoc2023

import aoc.utils._

class Day05(input: List[String]) {

	def parse(name: String): List[(Interval[Long], Long)] =
		input.dropWhile(!_.startsWith(name)).drop(1).takeWhile(!_.isEmpty).map { line =>
			val List(ds, ss, l) = words(line).map(_.toLong)
			(Interval(ss, (ss + l - 1)), ds - ss)
		}

	val seeds = input(0).drop(7).split(" ").map(_.toLong).toList
	val chain = List(
		parse("seed-to-soil"),
		parse("soil-to-fertilizer"),
		parse("fertilizer-to-water"),
		parse("water-to-light"),
		parse("light-to-temperature"),
		parse("temperature-to-humidity"),
		parse("humidity-to-location")
	)

	def apply(in: Interval[Long], mapping: Seq[(Interval[Long], Long)]): Seq[Interval[Long]] = {
		// for each domain in the mapping, we calculate what ranges from the input are mapped,
		// and apply the mapping to those input ranges
		val app = mapping.flatMap { case (domain, delta) =>
			in.intersect(domain).headOption.map(mapped => (mapped, Interval(mapped.start + delta, mapped.end + delta)))
		}
		// the output is the union of the mapping applied to the mapped ranges and the
		// unmapped ranges from the input
		app.map(_._2) ++ app.map(_._1).foldLeft(List(in)) { case (acc, m) => acc.flatMap(_ - m) }
	}

	def run(seeds: Seq[Interval[Long]]): Long =
		chain.foldLeft(seeds) { (acc, mapping) => Interval.merge(acc.map(apply(_, mapping)).flatten) }.map(_.start).min

	submit(1, run(seeds.map(s => Interval(s, s))))
	submit(2, run(seeds.grouped(2).map(p => Interval(p(0), p(0) + p(1) - 1)).toList))
}
