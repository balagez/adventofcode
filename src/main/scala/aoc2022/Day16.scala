package aoc2022

import aoc.utils.submit
import collection.mutable

/**
 * Disclaimer: This is not my solution, it's adapted to Scala from
 * https://github.com/betaveros/advent-of-code-2022/blob/main/p16.noul
 */
class Day16(input: List[String]) {

	val Line = "Valve ([A-Z]+)[^=]+=([0-9]+);[a-z ]+(.+)".r
	val valves = input.map { case Line(n, f, ns) => (n, f.toInt, ns.split(", ").toList) }
	val names = valves.map(_._1).toVector
	val flows = valves.map(_._2).toVector
	val neighbors = valves.map(_._3).toVector
	val aaIndex = names.indexOf("AA")
	val nonZeroFlows = flows.zipWithIndex.collect { case (f, i) if f > 0 => i }
	val indices = 0 to names.length - 1

	// Floyd-Warshall
	val dist = mutable.Map.empty[(Int, Int), Int].withDefaultValue(1000) // effectively 1000 = ∞
	for { u <- indices; n <- neighbors(u) } yield { dist.update(u -> names.indexOf(n), 1) }
	for { k <- indices; i <- indices; j <- indices } yield {
		val sum = dist(i -> k) + dist(k -> j)
		dist.updateWith(i -> j) {
			case Some(d) => Some(math.min(d, sum))
			case None => Some(sum)
		}
	}

	def chooseOne[A](xs: Vector[A]): Seq[(A, Vector[A])] = (0 to xs.length - 1).map { i =>
		(xs(i), xs.take(i) ++ xs.drop(i + 1))
	}

	val memo = mutable.HashMap.empty[(Int, Vector[Int], Int), Int]

	def dfs(cur: Int, rest: Vector[Int], t: Int): Int = memo.getOrElseUpdate((cur, rest, t), {
		((for { (r, rr) <- chooseOne(rest); if dist(cur -> r) < t } yield {
			flows(r) * (t - dist(cur -> r) - 1) + dfs(r, rr, t - dist(cur -> r) - 1)
		}) :+ 0).max
	})

	def dfs2(cur: Int, rest: Vector[Int], t: Int): Int =
		((for { (r, rr) <- chooseOne(rest); if dist(cur -> r) < t } yield {
			flows(r) * (t - dist(cur -> r) - 1) + dfs2(r, rr, t - dist(cur -> r) - 1)
		}) :+ dfs(aaIndex, rest, 26)).max

	submit(1, dfs(aaIndex, nonZeroFlows, 30))
	submit(2, dfs2(aaIndex, nonZeroFlows, 26))
}
