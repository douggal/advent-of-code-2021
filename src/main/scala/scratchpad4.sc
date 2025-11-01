val cs = Vector[Int](16,1,2,0,4,2,7,1,2,14)

val ds = for (i <- -100 to 100)  yield cs.map(c => Math.abs(c - i)).min

println(ds.mkString(", "))

ds.min

val s = "gcafex"
val x = s.toSet
val y = "gcxyz".toSet
x.intersect(y).count(_ => true)

s.toSet.equals("gcafe".toSet)
x.intersect(four).count(_ => true)