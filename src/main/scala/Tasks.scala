package aoc

import aoc.utils.{ ignore, measure }

import java.lang.reflect.InvocationTargetException
import scala.io.Source
import scala.util.{ Try, Failure }

sealed trait Task {

	def run(): Try[Unit]

	def inputFileFor(year: Int, day: Int): String = "aoc%d/day%02d.txt".format(year, day)
	def packageNameFor(year: Int): String = "aoc%d".format(year)
	def classNameFor(day: Int, variant: String): String = "Day%02d%s".format(day, variant)
}

final case class Init(year: Int, day: Int) extends Task {

	private def createFile(filename: String, content: String): Try[Unit] = Try {
		val file = new java.io.File(filename)
		if (!file.exists()) {
			file.createNewFile()
			val writer = new java.io.PrintWriter(file)
			try writer.write(content) finally writer.close()
		}
	}

	private def template(packageName: String, className: String): String = s"""package $packageName
		|
		|import aoc.utils._
		|
		|class $className(input: List[String]) {
		|
		|	submit(1, 0)
		|	submit(2, 0)
		|}
	""".stripMargin

	def run(): Try[Unit] = {
		val root = System.getProperty("user.dir")
		val packageName = packageNameFor(year)
		val className = classNameFor(day, variant = "")
		val inputFilename = s"$root/src/main/resources/${inputFileFor(year, day)}"
		val classFilename = s"$root/src/main/scala/aoc$year/$className.scala"
		for {
			_ <- createFile(inputFilename, "")
			_ <- createFile(classFilename, template(packageName, className))
		} yield ()
	}
}

final case class Run(year: Int, day: Int, variant: String, input: Option[String]) extends Task {

	def run(): Try[Unit] = for {
		input <- readInput(input, year, day)
		_ <- call(year, day, variant, input)
	} yield ()

	private def readInput(arg: Option[String], year: Int, day: Int): Try[List[String]] = {
		val filename = arg.getOrElse(inputFileFor(year, day))
		Try(Source.fromResource(filename).getLines().toList)
	}

	private def call(year: Int, day: Int, variant: String, input: List[String]): Try[Unit] =
		Try {
			val packageName = packageNameFor(year)
			val className = classNameFor(day, variant)
			val clazz = Class.forName(s"$packageName.$className")
			val constructor = clazz.getConstructor(classOf[List[String]])
			measure("Total time") {
				ignore(constructor.newInstance(input))
			}
		}.recoverWith {
			case e: InvocationTargetException => Failure(e.getCause)
		}
}
