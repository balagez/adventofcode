package aoc

import java.lang.IllegalArgumentException
import java.lang.reflect.InvocationTargetException
import scala.annotation.unused
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }
import aoc.utils.measure

object AdventOfCode {

	def main(args: Array[String]): Unit = {
		val t = for {
			(year, day, variant, inputOpt) <- parseArgs(args)
			input <- readInput(inputOpt, year, day)
			_ <- call(year, day, variant, input)
		} yield ()

		ignore(t.recover {
			case NonFatal(e) =>
				println(s"${Console.RED}ERROR:${Console.RESET} ${e.getClass.getSimpleName}: ${e.getMessage}")
		})
	}

	@inline private def ignore[A](@unused a: A): Unit = ()

	private val YearDay = "^([0-9]{4})/([0-9]{1,2}[a-z]{0,1})$".r

	private object AsInt {
		def unapply(s: String): Option[(Int, String)] = {
			val (nums, variant) = s.partition(_.isDigit)
			Try(nums.toInt).toOption.map(_ -> variant)
		}
	}

	private def parseArgs(args: Array[String]): Try[(Int, Int, String, Option[String])] =
		args match {
			case Array(YearDay(AsInt(year, _), AsInt(day, v)), input) => Success((year, day, v, Some(input)))
			case Array(YearDay(AsInt(year, _), AsInt(day, v))) => Success((year, day, v, None))
			case _ => Failure(new IllegalArgumentException(usage))
		}

	private def readInput(arg: Option[String], year: Int, day: Int): Try[List[String]] = {
		val filename = arg.getOrElse("aoc%d/day%02d.txt".format(year, day))
		Try(Source.fromResource(filename).getLines().toList)
	}

	private def call(year: Int, day: Int, variant: String, input: List[String]): Try[Unit] =
		Try {
			val packageName = s"aoc$year"
			val className = "Day%02d%s".format(day, variant)
			val clazz = Class.forName(s"$packageName.$className")
			val constructor = clazz.getConstructor(classOf[List[String]])
			measure("Total time") {
				ignore(constructor.newInstance(input))
			}
		}.recoverWith {
			case e: InvocationTargetException => Failure(e.getCause)
		}

	private val usage: String = "usage: yyyy/ddv [filename]"
}
