val l = Vector(22,13,17,11,0,8,2,23,4,24,21,9,14,16,7,6,10,3,18,5,1,12,20,15,19)
for (grp <- l.grouped(5)) println(grp)

val m = scala.collection.mutable.ListBuffer[Int]()
for (i <- 0 until 25) m += 0

for (i <- 5 until 10) m(i) = 1
m(0) = 1
m(4) = 1
m(9) = 1
m(14) = 1
m(19) = 1
m(24) = 1

l.zip(m).filter(i => if (i(1)==0) true else false).map(i => i(0))

println("Rows")
for (grp <- l.sliding(5,5).toList)
    println(grp)

println("Columns")
for (n <- 0 until 5)
    val grp = l.drop(n).grouped(5).map(_.head).toList
    println(grp)
    if (grp.toVector.sum == 5)
        println("bingo")

