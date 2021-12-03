val v = Vector[Int](200,300,299, 400,500)

def diff(v: Vector[Int]): Int = {
  v.reduceLeft(_ - _)
}

v.zip(v.drop(1)).map((a,b) => if ((b-a)>0) 1 else 0).sum

math.pow(2,3)