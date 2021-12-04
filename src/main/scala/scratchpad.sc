val l = " 1  2 34 99  3 42"
l.split("\\s+",-1).toList.drop(1).map(_.toInt)


