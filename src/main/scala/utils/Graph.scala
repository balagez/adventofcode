
package aoc.utils

object Graph {

	private final case class NodeWithCost[N](node: N, cost: Int)

	def dijkstra[N](source: N, successors: N => Iterable[(N, Int)], target: N => Boolean): Int = {
		val dist = collection.mutable.Map[N, Int](source -> 0).withDefault(_ => Int.MaxValue)
		val queue = collection.mutable.PriorityQueue(NodeWithCost(source, 0))(Ordering.by(-_.cost))
		val visited = collection.mutable.Set.empty[Int]

		while (queue.nonEmpty) {
			val u = queue.dequeue()
			visited += u.hashCode

			if (target(u.node)) return dist(u.node)

			successors(u.node).collect { case (v, d) if !visited.contains(v.hashCode) =>
				val alt = dist(u.node) + d
				if (alt < dist(v)) {
					dist.update(v, alt)
					queue.enqueue(NodeWithCost(v, alt))
				}
			}
		}
		0
	}
}