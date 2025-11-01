import scala.collection.mutable.ArrayBuffer

var fish = ArrayBuffer[Int](0,1,2,3,4,5,6)

for (f <- fish.grouped(3)) println(f.mkString(", "))

(for (nf <- fish) yield 8).toList

println(s"${fish.mkString(", ")} (start)")

val fishEnd = for (i <- 0 to 7) {
    val nfs = fish.count(f => f == 0)  // new fishes
    fish = fish.map(f => if (f == 0) 6 else f-1)
    fish ++= (for (nf <- 0 until nfs) yield 8).toList
    println(fish.mkString(", "))
}
println(s"final ${fish.mkString(", ")}")
