package aoc2023

import aoc.utils._

class Day07(input: List[String]) {

	val cards = "AKQJT98765432".toList
	val cr1 = cards.reverse.zipWithIndex.toMap.apply
	val cr2 = "AKQT98765432J".reverse.toList.zipWithIndex.toMap

	def hr(s: String): Int = s.groupBy(identity).map(_._2.length).toList.sorted match {
		case List(5) => 6 // Five of a kind
		case List(1, 4) => 5 // Four of a kind
		case List(2, 3) => 4 // Full house
		case List(1, 1, 3) => 3 // Three of a kind
		case List(1, 2, 2) => 2 // Two pair
		case List(1, 1, 1, 2) => 1 // One pair
		case _ => 0 // High card
	}

	case class Hand(v: String) {
		val rank1 = (hr(v), cr1(v(0)), cr1(v(1)), cr1(v(2)), cr1(v(3)), cr1(v(4)))
		val rank2 = (cards.map(c => hr(v.replace('J', c))).max, cr2(v(0)), cr2(v(1)), cr2(v(2)), cr2(v(3)), cr2(v(4)))
	}

	val bids = input.map(_.split(" ") match { case Array(hand, bid) => (Hand(hand), bid.toLong) })

	def winnings(ord: Ordering[Hand]): Long =
		bids.sortBy(_._1)(ord).zipWithIndex.map { case ((_, bid), i) => bid * (i + 1) }.sum

	submit(1, winnings(Ordering.by(_.rank1)))
	submit(2, winnings(Ordering.by(_.rank2)))
}
