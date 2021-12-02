import scala.annotation.tailrec

object Day02 extends App {
  // created 12/02/2021
  // https://adventofcode.com/2021/day/2

  println(s"--- Day 2: Dive! ---")

  val filename = "Day02Input.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val course = bufferedSource
    .getLines
    .map(line => line.split(' ')) // split on char trims whitespace, regex doesn't
    .map(e => (e(0).toLowerCase, e(1).toInt)) // convert Array from split into Tuple2 of (String, Int)
    .toSeq

  // debug check what course looks like
  // course = Seq[Tuple2(String, Int)]
  course.foreach(d => println(s"${d(0)}: ${d(1)}"))

  // location = (x,y) = (forward distance units, depth in units)
  var loc = (0,0)
  @tailrec
  def navigate(course: Seq[(String, Int)]): Unit = {

    if (course.isEmpty) ()
    else {
      val (c:String, x:Int) = course.head  // c = nav command, x = units
      c match {
        case "forward" =>
          loc = (loc._1+x,loc._2)
          navigate(course.tail)

        case "down" =>
          loc = (loc._1,loc._2+x) // from the hint, down = depth increases, gets bigger
          navigate(course.tail)

        case "up" =>
          loc = (loc._1,loc._2-x) // from the hint, down = depth decreases, gets smaller
          navigate(course.tail)

        case _ => println("Error"); ()
      }
    }
  }

  navigate(course)

  println(s"Location (forward units, depth units) $loc")
  println(s"Part 1 answer forward units x depth units: ${loc._1*loc._2}")

  bufferedSource.close

}
