package aoc

import scala.util.Try

object utils {

	private val green: String = "\u001B[38;5;118m"
	private val gray: String = "\u001B[38;5;60m"
	private val reset: String = Console.RESET

	@inline def measure[A](label: String)(block: => A): A = {
		val start = System.nanoTime
		val res = block
		val end = System.nanoTime
		println(s"$gray[%s: %5.6f ms]$reset".format(label, BigDecimal(end - start) / 1000000))
		res
	}

	@inline def submit(num: Int, result: Any): Unit = {
		println(s"$num. $green$result$reset")
	}

	def words(s: String): List[String] = s.split("\\s+").toList

	implicit class StringParsingOps(val s: String) extends AnyVal {

		@inline def long: Long = s.dropWhile(!_.isDigit).takeWhile(_.isDigit).toLong
		@inline def longOpt: Option[Long] = Try(long).toOption
		@inline def longList(sep: String): List[Long] = s.dropWhile(!_.isDigit).split(sep).map(_.toLong).toList

		@inline def int: Int = long.toInt
		@inline def intOpt: Option[Int] = Try(int).toOption
		@inline def intList(sep: String): List[Int] = longList(sep).map(_.toInt)
	}
}
