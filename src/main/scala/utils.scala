package aoc

import scala.annotation.unused
import scala.util.Try

package object utils {

	@inline def ignore[A](@unused a: A): Unit = ()

	@inline def measure[A](label: String)(block: => A): A = {
		val start = System.nanoTime
		val res = block
		val end = System.nanoTime
		println(s"${Colors.gray}[%s: %5.6f ms]${Colors.reset}".format(label, BigDecimal(end - start) / 1000000))
		res
	}

	@inline def submit(num: Int, result: Any): Unit = {
		println(s"$num. ${Colors.green}$result${Colors.reset}")
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
