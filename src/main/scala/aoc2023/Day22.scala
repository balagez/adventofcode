package aoc2023

import aoc.utils._
import scala.annotation.tailrec

class Day22(input: List[String]) {

	case class Brick(index: Int, ax: Int, ay: Int, az: Int, bx: Int, by: Int, bz: Int, supports: Set[Int], restsOn: Set[Int]) {
		val (minz, maxz) = (az min bz, az max bz)
		def intersectsWith(that: Brick): Boolean = !(bx < that.ax || ax > that.bx) && !(by < that.ay || ay > that.by)
		def dropTo(newz: Int): Brick = copy(az = az + newz - minz, bz = bz + newz - minz)
	}

	val bricks = input.zipWithIndex.map { case (line, i) =>
		val Array(ax, ay, az, bx, by, bz) = line.split("[~,]").map(_.toInt)
		Brick(i, ax, ay, az, bx, by, bz, Set(), Set())
	}

	def settle(bricks: List[Brick]): List[Brick] = {
		@tailrec def drop(falling: List[Brick], settled: List[Brick]): List[Brick] = falling match {
			case Nil => settled
			case current :: rest =>
				val underneath = settled.filter(_.intersectsWith(current))
				val (dropped, restsOn) = underneath.map(_.maxz).maxOption match {
					case None => (current.dropTo(1), Nil)
					case Some(maxz) => (current.dropTo(maxz + 1), underneath.filter(_.maxz == maxz))
				}
				// link the currently dropped one with the ones it rests on
				drop(falling = rest, settled = dropped.copy(restsOn = restsOn.map(_.index).toSet) ::
					restsOn.map(that => that.copy(supports = that.supports + dropped.index)) :::
					settled.filterNot(restsOn.contains))
		}
		// 1. the lowest can be dropped without collision; 2. the ones on the floor are already settled
		drop.tupled(bricks.sortBy(_.minz).partition(_.minz > 1))
	}

	val settled = settle(bricks)
	val lookup = settled.map(b => b.index -> b).toMap
	val (safe, unsafe) = settled.partition(_.supports.forall(lookup(_).restsOn.sizeIs > 1))

	@tailrec private def bfs(queue: Set[Int], disintegrated: Set[Int]): Int =
		if (queue.isEmpty) disintegrated.size - 1
		else {
			val index = queue.head
			val newDisintegrated = disintegrated + index
			val supported = lookup(index).supports
			if (supported.nonEmpty) {
				val willFall = supported
					.filterNot(newDisintegrated.contains)
					.filter(i => lookup(i).restsOn.forall(newDisintegrated.contains))
				bfs(queue.tail ++ willFall, newDisintegrated)
			} else {
				bfs(queue.tail, newDisintegrated)
			}
		}

	submit(1, safe.size)
	submit(2, unsafe.map(brick => bfs(Set(brick.index), Set())).sum)
}
