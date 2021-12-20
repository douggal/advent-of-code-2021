case class Point(row:Int, col:Int)

val a = Point(1,2)
val b = Point(3,4)
val x = List[Point](a,b)

x.contains(Point(3,4))

val np = Point(a.row+1,a.col)
println(np)

// https://stackoverflow.com/questions/11448685/scala-how-can-i-count-the-number-of-occurrences-in-a-list?noredirect=1&lq=1
//val l = List("a","b","cc","cc","AA","dd","BB","yy","xx") ::: List("dd")
val l = List("start","c","c","c") ::: List("dd")
val test0 = l.filter(a => a != a.capitalize)
val test = l.groupBy(identity).map(t => (t._1, t._2.length))
// https://stackoverflow.com/questions/7802851/whats-the-best-way-to-inverse-sort-in-scala
test.values.toList.count(_ > 1)
