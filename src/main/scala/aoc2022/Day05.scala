package aoc2022

import aoc.utils.submit

class Day05(input: List[String]) {

	import scala.collection.mutable.Stack

	val stackCount = (input.head.length + 1) / 4
	val stacksA = List.fill(stackCount)(Stack.empty[Char])
	val stacksB = List.fill(stackCount)(Stack.empty[Char])

	input.collect {
		case line if line.contains("[") =>
			(0 to stackCount - 1).foreach(i =>
				line(1 + i * 4) match {
					case ' ' => ()
					case item =>
						stacksA(i).append(item)
						stacksB(i).append(item)
				}
			)
		case line if line.startsWith("move") =>
			val Array(count, from, to) = line.split(" ").filter(_.forall(_.isDigit)).map(_.toInt - 1)
			val buffer = Stack.empty[Char]
			(0 to count).foreach { _ =>
				stacksA(to).push(stacksA(from).pop())
				buffer.push(stacksB(from).pop())
			}
			stacksB(to).pushAll(buffer)
	}

	submit(1, stacksA.map(_.head).mkString)
	submit(2, stacksB.map(_.head).mkString)
}
