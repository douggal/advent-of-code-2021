case class Point(row:Int, col:Int)

val a = Point(1,2)
val b = Point(3,4)
val x = List[Point](a,b)

x.contains(Point(3,4))

val np = Point(a.row+1,a.col)
println(np)
