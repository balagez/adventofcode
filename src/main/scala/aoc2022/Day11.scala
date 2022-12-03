package aoc2022

import aoc.utils._

class Day11(input: List[String]) {

	case class Monkey(num: Int, worries: List[Long], op: Long => Long, test: Long, ifTrue: Int, ifFalse: Int, inspected: Long)

	def op(c: Char, arg: Option[Long]): (Long => Long) = c match {
		case '+' => x => x + arg.getOrElse(x)
		case '*' => x => x * arg.getOrElse(x)
	}

	val atStart = input.foldLeft(Vector.empty[Monkey]) {
		case (acc, l) if l.startsWith("M") => Monkey(l.int, List(), identity, 0, 0, 0, 0) +: acc
		case (m +: r, l) if l.startsWith("  S") => m.copy(worries = l.longList(", ").toList) +: r
		case (m +: r, l) if l.startsWith("  O") => m.copy(op = op(l(23), l.longOpt)) +: r
		case (m +: r, l) if l.startsWith("  T") => m.copy(test = l.long) +: r
		case (m +: r, l) if l.startsWith("    If t") => m.copy(ifTrue = l.int) +: r
		case (m +: r, l) if l.startsWith("    If f") => m.copy(ifFalse = l.int) +: r
		case (acc, _) => acc
	}.reverse

	val monkeyIterator = 0 to atStart.length - 1
	val testSpace = atStart.map(_.test).product

	def run(rounds: Int, partOp: Long => Long): Long =
		(1 to rounds).foldLeft(atStart) { (atRound, _) =>
			monkeyIterator.foldLeft(atRound) { (atTurn, monkeyNum) =>
				val Monkey(num, worries, op, test, ifTrue, ifFalse, inspected) = atTurn(monkeyNum)
				val throws = worries.map { worry =>
					val newWorry = partOp(op(worry))
					val toMonkey = if (newWorry % test == 0) ifTrue else ifFalse
					(toMonkey, newWorry)
				}
				atTurn.map {
					case m if m.num == num => m.copy(worries = List(), inspected = inspected + worries.length)
					case m => m.copy(worries = m.worries ++ throws.collect { case (m.num, worry) => worry })
				}
			}
		}.map(_.inspected).sortBy(-_).take(2).product

	submit(1, run(20, _ / 3))
	submit(2, run(10000, _ % testSpace))
}
