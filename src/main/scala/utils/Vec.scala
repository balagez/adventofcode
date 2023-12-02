package aoc.utils

import scala.math.{ abs, sqrt, pow }

final case class Vec(x: Int, y: Int) {
	override def toString: String = s"($x,$y)"
	def +(p: Vec): Vec = Vec(x + p.x, y + p.y)
	def -(p: Vec): Vec = Vec(x - p.x, y - p.y)
	def *(i: Int): Vec = Vec(x * i, y * i)
	def isNextTo(p: Vec): Boolean = abs(x - p.x) <= 1 && abs(y - p.y) <= 1
	def signum: Vec = Vec(x = scala.math.signum(x), y = scala.math.signum(y))
	def distanceTo(p: Vec): Int = sqrt(pow(x.toDouble - p.x, 2) + pow(y.toDouble - p.y, 2)).toInt
	def %%(p: Vec): Vec = Vec(x % p.x, y % p.y)
}

object Vec {
	val zero = Vec(0, 0)
	val up = Vec(0, -1)
	val down = Vec(0, 1)
	val left = Vec(-1, 0)
	val right = Vec(1, 0)
	val dirs = List(up, down, left, right)
}
