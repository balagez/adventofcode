package aoc

import java.lang.IllegalArgumentException
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }
import aoc.utils.ignore

object AdventOfCode {

	def main(args: Array[String]): Unit = {
		val t = for {
			task <- parseArgs(args)
			_ <- task.run()
		} yield ()

		ignore(t.recover {
			case NonFatal(e) =>
				println(s"${Console.RED}ERROR:${Console.RESET} ${e.getClass.getSimpleName}: ${e.getMessage}")
		})
	}

	private val YearDay = "^([0-9]{4})/([0-9]{1,2}[a-z]{0,1})$".r

	private object AsInt {
		def unapply(s: String): Option[(Int, String)] = {
			val (nums, variant) = s.partition(_.isDigit)
			Try(nums.toInt).toOption.map(_ -> variant)
		}
	}

	private def parseArgs(args: Array[String]): Try[Task] =
		args match {
			case Array(YearDay(AsInt(year, _), AsInt(day, _)), "init") => Success(Init(year, day))
			case Array(YearDay(AsInt(year, _), AsInt(day, v)), input) => Success(Run(year, day, v, Some(input)))
			case Array(YearDay(AsInt(year, _), AsInt(day, v))) => Success(Run(year, day, v, None))
			case _ => Failure(new IllegalArgumentException(usage))
		}

	private val usage: String = "usage: yyyy/ddv [filename]"
}
