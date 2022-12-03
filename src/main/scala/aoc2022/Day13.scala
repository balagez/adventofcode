package aoc2022

import aoc.utils.submit

class Day13(input: List[String]) {

	@inline def numericPrefix(s: List[Char]): (Int, Int) = {
		val digits = s.takeWhile(_.isDigit)
		(digits.mkString.toInt, digits.length)
	}

	@inline private def wrap(s: List[Char]): List[Char] = {
		val digits = s.takeWhile(_.isDigit)
		('[' +: digits :+ ']') ++ s.drop(digits.length)
	}

	@annotation.tailrec final def compare(l: List[Char], r: List[Char]): Boolean = (l, r) match {
		case ('[' :: ltail, '[' :: rtail) => compare(ltail, rtail)
		case (']' :: ltail, ']' :: rtail) => compare(ltail, rtail)
		case (',' :: ltail, ',' :: rtail) => compare(ltail, rtail)
		// one of them ran out
		case (Nil, _) => true
		case (_, Nil) => false
		case (']' :: _, _) => true
		case (_, ']' :: _) => false
		// wrapping
		case ('[' :: _, _) => compare(l, wrap(r))
		case (_, '[' :: _) => compare(wrap(l), r)
		// two numbers
		case _ =>
			val (lint, llen) = numericPrefix(l)
			val (rint, rlen) = numericPrefix(r)
			if (lint < rint) true
			else if (lint > rint) false
			else compare(l.drop(llen), r.drop(rlen))
	}

	val in = input.filter(_.nonEmpty).map(_.toList)
	val divs = List("[[2]]".toList, "[[6]]".toList)
	val sorted = (in ++ divs).sortWith(compare)

	submit(1, in.grouped(2).map(lr => compare(lr(0), lr(1))).zipWithIndex.collect { case (true, i) => i + 1 }.sum)
	submit(2, divs.map(sorted.indexOf(_) + 1).product)
}
