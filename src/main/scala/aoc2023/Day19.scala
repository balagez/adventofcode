package aoc2023

import aoc.utils._
import scala.annotation.tailrec

class Day19(input: List[String]) {

	sealed trait Decision
	case object A extends Decision // Accept
	case object R extends Decision // Reject
	case class Jump(next: String) extends Decision

	object Decision { def apply(s: String): Decision = s match { case "A" => A; case "R" => R; case _ => Jump(s) } }

	type Part = Map[String, Int]

	case class Workflow(name: String, rules: List[Rule], combinations: Part = Map.empty)

	trait Rule extends Function1[Part, Option[Decision]]
	case class Gt(cat: String, value: Int, decision: Decision) extends Rule {
		def apply(part: Part): Option[Decision] = Option.when(part(cat) > value)(decision)
	}
	case class Lt(cat: String, value: Int, decision: Decision) extends Rule {
		def apply(part: Part): Option[Decision] = Option.when(part(cat) < value)(decision)
	}
	case class Unconditional(decision: Decision) extends Rule {
		def apply(part: Part): Option[Decision] = Some(decision)
	}

	val workflows = input.takeWhile(_.nonEmpty).map { line =>
		val Array(name, rest) = line.split("\\{")
		name -> Workflow(name, rest.init.split(",").toList.map(s =>
			(s.split(":") match {
				case Array(op, label) =>
					val (cat, value) = (op.takeWhile(_.isLetter), op.filter(_.isDigit).toInt)
					if (op.contains(">")) {
						Gt(cat, value, Decision(label))
					} else {
						Lt(cat, value, Decision(label))
					}
				case Array(label) => Unconditional(Decision(label))
			}): Rule
		))
	}.toMap

	val parts = input.dropWhile(_.nonEmpty).drop(1).map { line =>
		line.drop(1).init.split(",").map(
			_.split("=") match { case Array(name, value) => (name, value.toInt) }
		).toMap
	}

	def run(part: Part): Boolean = {
		@tailrec def loop(current: String): Decision =
			workflows(current).rules.collectFirst { case rule if rule(part).isDefined => rule(part).get } match {
				case Some(A) => A
				case Some(Jump(next)) => loop(next)
				case _ => R
			}
		loop("in") == A
	}

	def combinations(workflow: Workflow, ranges: Map[String, Interval[Int]]): Long = {

		def eval(decision: Decision, here: Map[String, Interval[Int]]): Long = decision match {
			case A => here.values.map(_.length.toLong).product
			case R => 0
			case Jump(next) => combinations(workflows(next), here)
		}

		workflow.rules.foldLeft((ranges, 0L)) {
			case ((acc, sum), Lt(cat, value, decision)) =>
				val here = acc.updated(cat, acc(cat).intersect(Interval(1, value - 1)).head)
				val rest = acc.updated(cat, acc(cat).intersect(Interval(value, 4000)).head)
				(rest, sum + eval(decision, here))
			case ((acc, sum), Gt(cat, value, decision)) =>
				val here = acc.updated(cat, acc(cat).intersect(Interval(value + 1, 4000)).head)
				val rest = acc.updated(cat, acc(cat).intersect(Interval(1, value)).head)
				(rest, sum + eval(decision, here))
			case ((acc, sum), Unconditional(decision)) =>
				(Map(), sum + eval(decision, acc))
		}._2
	}

	val all = Interval(1, 4000)

	submit(1, parts.filter(run).flatMap(_.values).sum)
	submit(2, combinations(workflows("in"), Map("x" -> all, "m" -> all, "a" -> all, "s" -> all)))
}
