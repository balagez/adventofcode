package aoc2023

import aoc.utils._

class Day15(input: List[String]) {

	def hash(s: String): Int = s.foldLeft(0)((acc, i) => ((acc + i.toInt) * 17) % 256)

	val initSequence = input.flatMap(_.split(","))

	case class Lens(label: String, focal: Int)

	val boxes = initSequence.foldLeft(Map.empty[Int, List[Lens]]) { (boxes, step) =>
		val label = step.takeWhile(_.isLetter)
		val index = hash(label)
		val box = boxes.get(index)
		step(label.length) match {
			case '-' =>
				box match {
					case None => boxes
					case Some(lenses) => boxes.updated(index, lenses.filterNot(_.label == label))
				}
			case '=' =>
				val lens = Lens(label, focal = step.drop(label.length + 1).toInt)
				box match {
					case None => boxes.updated(index, List(lens))
					case Some(lenses) =>
						boxes.updated(index, lenses.indexWhere(_.label == label) match {
							case -1 => lenses.appended(lens)
							case i => lenses.updated(i, lens)
						})
				}
		}
	}

	submit(1, initSequence.map(hash).sum)
	submit(2, boxes.flatMap { case (b, ls) => ls.zipWithIndex.map { case (l, i) => (1 + b) * (i + 1) * l.focal } }.sum)
}
