package aoc2022

import aoc.utils.submit

class Day08(input: List[String]) {

	import scala.annotation.tailrec

	val grid = input.map(_.toVector.map(_.toString.toInt)).toVector
	val gridT = grid.transpose
	val w = grid.head.length
	val h = grid.length

	@inline @tailrec final def countShorter(height: Int, list: Seq[Int], acc: Int = 0): Int =
		list match {
			case Seq() => acc
			case head +: _ if head >= height => acc + 1
			case _ +: tail => countShorter(height, tail, acc + 1)
		}

	val res = for {
		y <- 1 to h - 2
		x <- 1 to w - 2
		current = grid(y)(x)

		left = grid(y).take(x)
		right = grid(y).drop(x + 1)
		top = gridT(x).take(y)
		bottom = gridT(x).drop(y + 1)
		visible = if (left.max < current || right.max < current || top.max < current || bottom.max < current) 1 else 0

		scoreLeft = countShorter(current, left.reverse)
		scoreRight = countShorter(current, right)
		scoreTop = countShorter(current, top.reverse)
		scoreBottom = countShorter(current, bottom)
		score = scoreLeft * scoreRight * scoreTop * scoreBottom
	} yield (visible, score)

	submit(1, res.map(_._1).sum + 2 * (w + h) - 4)
	submit(2, res.map(_._2).max)
}
