import java.util.concurrent._

import scala.io.Source

object MyUtils {
  // The fork/join framework uses a work-stealing algorithm
  // http://supertech.csail.mit.edu/papers/steal.pdf
  val forkJoinPool = new ForkJoinPool

  def task[T](computation: => T): RecursiveTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = computation
    }

    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork() // schedule for execution
      case _ =>
        forkJoinPool.execute(t)
    }

    t
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task { taskB }
    val left = taskA

    (left, right.join())
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
  def readFromFile(path: String): (Int, List[(Int, Int, Int)]) = {
    var points: List[(Int, Int, Int)] = List()
    val iterator = Source.fromFile(path).getLines()
    val num_vertices = iterator.next().toInt
    //println(num_vertices)
    for (line <- iterator){

      //println("Line:", line)

      val new_point = line.split(' ') match {
        case Array(x, y, z) => (x.toInt, y.toInt, z.toInt)
      }
      points = new_point :: points

    }
    (num_vertices, points)
  }
}