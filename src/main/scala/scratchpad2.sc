val xs = Vector(1,2,3,4,5)
val ys = Vector(9,9,9,9,9)
val zs = Vector(1,1,1,1,1)

val xy = for {
    i1 <- xs
    i2 <- ys
} yield (i1, i2)
val t = (3,9)

val points = xs.zip(ys)

val t = (3,9)

if (xs.zip(ys).contains(t)) println("bingo")

val q = xs.zip(ys).zipWithIndex.filter(f => {if (f._1._1==t._1 && f._1._2==t._2) true else false})
zs(q.head._2) + 1