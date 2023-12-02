package aoc2023

import aoc.utils._
import aoc.utils.Math.leastCommonMultiple
import scala.annotation.tailrec

class Day20(input: List[String]) {

	type Name = String
	type Pulse = Boolean
	type Out = (String, Pulse)

	sealed trait Module extends Function2[Name, Pulse, List[Out]] { val outs: List[Name] }
	case class FlipFlop(outs: List[Name]) extends Module {
		private var on: Boolean = false
		def apply(from: Name, p: Pulse): List[Out] = if (p) Nil else { on = !on; outs.map(_ -> on) }
	}
	case class Conjunction(outs: List[Name], ins: List[Name]) extends Module {
		private val state: collection.mutable.Map[Name, Boolean] = ins.map(_ -> false).to(collection.mutable.HashMap)
		def apply(from: Name, p: Pulse): List[Out] = {
			state.update(from, p)
			val out = !state.values.forall(identity)
			outs.map(_ -> out)
		}
	}
	case class Broadcaster(outs: List[Name]) extends Module {
		def apply(from: Name, p: Pulse): List[Out] = outs.map(_ -> p)
	}

	val unlinked: Map[Name, Module] = input.map { line =>
		val Array(name, outss) = line.split(" -> ")
		val outs = outss.split(", ").toList
		name.head match {
			case 'b' => name -> Broadcaster(outs)
			case '%' => name.tail -> FlipFlop(outs)
			case '&' => name.tail -> Conjunction(outs, Nil)
		}
	}.toMap

	val network = unlinked ++ unlinked.collect { case (name, module: Conjunction) =>
		name -> module.copy(ins = unlinked.collect { case (n, m) if m.outs.contains(name) => n }.toList)
	}

	val (mainName, mainModule) = network.collectFirst { case (n, m: Conjunction) if m.outs.contains("rx") => (n, m) }.get
	val cycles = mainModule.ins.map(_ -> 0).to(collection.mutable.Map)

	var lows = 0L
	var highs = 0L
	var push = 0
	var continue = true
	while (continue) {
		push = push + 1
		@tailrec def loop(steps: List[(Name, List[Out])]): Unit = steps match {
			case Nil => ()
			case _ =>
				val newouts = steps.flatMap { case (from, outs) =>
					outs.map { case (to, p) =>
						if (push <= 1000) {
							if (p) highs = highs + 1 else lows = lows + 1
						}
						if (to == mainName && p && cycles(from) == 0) cycles.update(from, push)
						to -> (network.get(to) match {
							case None => List()
							case Some(module) => module.apply(from, p)
						})
					}
				}
				loop(newouts)
		}
		loop(List("button" -> List("broadcaster" -> false)))

		if (cycles.forall(_._2 > 0)) continue = false
	}

	submit(1, lows * highs)
	submit(2, cycles.values.toList.map(_.toLong).reduce(leastCommonMultiple))
}
