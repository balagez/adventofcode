package aoc.utils

trait ToChar[T] {
	def toChar(t: T): Char
}

object ToChar {
	def apply[A](fn: A => Char): ToChar[A] = new ToChar[A] { def toChar(a: A): Char = fn(a) }
}

trait Show {

	implicit val charToChar: ToChar[Char] = new ToChar[Char] { def toChar(c: Char): Char = c }

	def show[T](map: collection.Map[Vec, T], empty: Char = '.')(implicit ev: ToChar[T]): Unit = if (map.nonEmpty) {
		val xs = map.keys.map(_.x)
		val ys = map.keys.map(_.y)
		val (minx, maxx, miny, maxy) = (xs.min, xs.max, ys.min, ys.max)
		(miny to maxy).foreach { y =>
			(minx to maxx).foreach { x =>
				print(map.get(Vec(x, y)).map(ev.toChar).getOrElse(empty))
				if (x == maxx) println()
			}
		}
	}

	def show[T](map: Iterable[Iterable[T]])(implicit ev: ToChar[T]): Unit =
		if (map.nonEmpty && map.head.nonEmpty) {
			map.foreach(line => println(line.map(ev.toChar).mkString))
		}
}
