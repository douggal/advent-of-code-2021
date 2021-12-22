val x = 6

(x + 3 +2 + 1)% 10

class detDie() {
    var N: Int = 0
    var faceValue: Int = 0

    def roll(): Int = {
        faceValue += 1
        N += 1
        faceValue
    }
    override def toString: String =
        s"Nbr rolls is ($N)"
}

val y = new detDie()
y.roll()
y.roll()
y.roll()
y.toString

class CircularTrack(var x: Int) {
    def move(dx: Int): Unit = {
        val m = (x + dx) % 10
        if (m==0) x=1 else x=m
    }
    override def toString: String =
        s"Board position ($x)"
}
val p = new CircularTrack(4)
p.move(6)
p.x
9 % 9
