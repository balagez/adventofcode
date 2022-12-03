package aoc2022

import aoc.utils.submit

class Day07(input: List[String]) {

	import scala.collection.mutable.{ Stack, Map }

	val currentPath = Stack.empty[String]
	val sizes = Map.empty[String, Int]

	input.foreach {
		case "$ cd /" => currentPath.removeAll()
		case "$ cd .." => currentPath.pop()
		case line if line.startsWith("$ cd ") => currentPath.push(line.drop(5))
		case line if line.startsWith("$") => ()
		case line if line.startsWith("dir ") => ()
		case line =>
			val size = line.split(" ").head.toInt
			("" +: currentPath.toList.reverse).foldLeft("") { (prefix, dir) =>
				val path = prefix + "/" + dir
				sizes.updateWith(path) {
					case Some(sum) => Some(sum + size)
					case None => Some(size)
				}
				path
			}
	}

	val freeNeeded = 30000000
	val freeNow = 70000000 - sizes.getOrElse("/", 0)

	submit(1, sizes.collect { case (_, size) if size <= 100000 => size }.sum)
	submit(2, sizes.collect { case (_, size) if freeNow + size >= freeNeeded => size }.min)
}
