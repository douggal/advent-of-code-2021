case class Point(row:Int, col:Int)

val a = Point(1,2)
val b = Point(3,4)
val x = List[Point](a,b)

x.contains(Point(3,4))

val np = Point(a.row+1,a.col)
println(np)

// https://stackoverflow.com/questions/11448685/scala-how-can-i-count-the-number-of-occurrences-in-a-list?noredirect=1&lq=1
val l = List("a","b","cc","cc","dd","dd")
val test = l.groupBy(identity).map(t => (t._1, t._2.length))
// https://stackoverflow.com/questions/7802851/whats-the-best-way-to-inverse-sort-in-scala
test.values.toList.sortBy(- _).drop(1).count(l => l > 1)
