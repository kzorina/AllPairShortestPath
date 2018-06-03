import MyUtils._
import Array._

object Main {
  val nWorkers = 2 // power of 2

  def print_matrix(matrix: Array[Array[Int]]) = {
    for (i <- matrix.indices) {
      for ( j <- matrix(0).indices) {
        if (matrix(i)(j) == 100000)
          print(" inf")
        else
          print("   " + matrix(i)(j))
      }
      println()
    }
  }

  def compute_start_matr(graph: List[(Int, Int, Int)], vertices: Int): Array[Array[Int]] ={
    var matrix = ofDim[Int](vertices, vertices)
    for (i <- 0 until vertices)
      for ( j <- 0 until vertices)
        if (i == j){
          matrix(i)(j) = 0
        } else {
          matrix(i)(j) = 100000
        }
    for (k <- graph.indices)
      matrix(graph(k)._1 - 1)(graph(k)._2 - 1) = graph(k)._3
    //print_matrix(matrix)
    matrix

  }

  def compute_shortest_path_basic(input_matrix: Array[Array[Int]], vertices: Int): Array[Array[Int]] = {
    val new_matrix = ofDim[Int](input_matrix.length, input_matrix.length)
    var matrix = input_matrix
    for (k <- 0 until vertices) {
     // println("Step "+(k+1))

      for (i <- input_matrix.indices) {
        for (j <- input_matrix.indices) {
          new_matrix(i)(j) = Math.min(matrix(i)(j), matrix(i)(k) + matrix(k)(j))
        }
      }
      //print_matrix(new_matrix)
      matrix = new_matrix
    }
    new_matrix
  }



  def combine(matrix: Array[Array[Int]], up1: Array[Array[Int]], up2: Array[Array[Int]], down1: Array[Array[Int]], down2: Array[Array[Int]]) = {
    for (i <- up1.indices) {
      for (j <- up1(0).indices)
        matrix(i)(j) = up1(i)(j)
      for (j <- up2(0).indices)
        matrix(i)(j + up1(0).length) = up2(i)(j)
    }
    for (i <- down1.indices) {
      for (j <- down1(0).indices)
        matrix(i + up1.length)(j) = down1(i)(j)
      for (j <- down2(0).indices)
        matrix(i + up1.length)(j + down1(0).length) = down2(i)(j)
    }
    matrix
  }
  def parallel_step(matrix: Array[Array[Int]], nWorkers: Int, i_part: (Int, Int),
                    j_part: (Int, Int), k: Int): Array[Array[Int]] = nWorkers match {
    case 1 =>
      val new_matrix = ofDim[Int](i_part._2 - i_part._1, j_part._2 - j_part._1)
      for (i <- 0 until i_part._2 - i_part._1)
        for (j <- 0 until j_part._2 - j_part._1)
          new_matrix(i)(j) = matrix(i_part._1 + i)(j_part._1 + j)

      for (i <- 0 until i_part._2 - i_part._1)
        for (j <- 0 until j_part._2 - j_part._1)
          new_matrix(i)(j) = Math.min(new_matrix(i)(j), matrix(i + i_part._1)(k) + matrix(k)(j + j_part._1))
      new_matrix
    case 2 =>
      val (up, down) = parallel(parallel_step(matrix, nWorkers / 2, (i_part._1, i_part._2 / 2), (j_part._1, j_part._2), k),
        parallel_step(matrix, nWorkers / 2, (i_part._2 / 2, i_part._2), (j_part._1, j_part._2),  k))
      up ++ down
    case 4 =>
      val (up1, up2, down1, down2) = parallel(
        parallel_step(matrix, nWorkers / 4, (i_part._1, i_part._2 / 2), (j_part._1, j_part._2 / 2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._1, i_part._2 / 2), (j_part._2 / 2, j_part._2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._2 / 2, i_part._2), (j_part._1, j_part._2 / 2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._2 / 2, i_part._2), (j_part._2 / 2,j_part._2),  k)
      )
      combine(matrix, up1, up2, down1, down2)
    case _ =>
      val (up1, up2, down1, down2) = parallel(
        parallel_step(matrix, nWorkers / 4, (i_part._1, i_part._2 / 2), (j_part._1, j_part._2 / 2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._1, i_part._2 / 2), (j_part._2 / 2, j_part._2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._2 / 2, i_part._2), (j_part._1, j_part._2 / 2),  k),
        parallel_step(matrix, nWorkers / 4, (i_part._2 / 2, i_part._2), (j_part._2 / 2,j_part._2),  k)
      )
      combine(matrix, up1, up2, down1, down2)
  }
  def compute_shortest_path_par(input_matrix: Array[Array[Int]], nWorkers: Int): Array[Array[Int]] = {
    var matrix = input_matrix
    for (k <- input_matrix.indices) {
      //println("Step "+(k+1))
      matrix = parallel_step(matrix, nWorkers, (0, input_matrix.length), (0, input_matrix.length), k)
      //print_matrix(matrix)
      //println()
    }
    matrix
  }

  def main(args: Array[String]): Unit = {
    val (vertices, points) =  readFromFile("input.txt")

    val starting_matrix = compute_start_matr(points, vertices)
    println("Starting matrix: ")
    print_matrix(starting_matrix)
    val t0 = System.nanoTime()
    val path_matrix = compute_shortest_path_basic(starting_matrix, vertices)
    val t1 = System.nanoTime()
    println("Elapsed time (non parallel): "+(t1-t0) + " ns")

    println("Output (non par): ")
    print_matrix(path_matrix)
    val t3 = System.nanoTime()
    val path_matrix_par = compute_shortest_path_par(starting_matrix,  nWorkers)
    val t4 = System.nanoTime()

    println("Elapsed time (parallel): "+(t4-t3) + " ns")
    println("Output (par): ")
    print_matrix(path_matrix_par)

    println("Speedup with parallel: "+(t1-t0)/(t4-t3).toDouble)
    // No effect of speedup with small graphs because of all the assigning operations
  }
}




/*
def compute_shortest_path_chunk(input_matrix: Array[Array[Int]], i_part: (Int, Int),
                                    j_part: (Int, Int), verbose:Boolean, k:Int): Array[Array[Int]] = {


      val new_matrix = ofDim[Int](i_part._2 - i_part._1, j_part._2 - j_part._1)
      for (i <- 0 until i_part._2 - i_part._1)
        for (j <- 0 until j_part._2 - j_part._1)
          new_matrix(i)(j) = input_matrix(i_part._1 + i)(j_part._1 + j)
      if (verbose) {
        println("i part: " + i_part)
        println("j part: " + j_part)
      }
      for (i <- 0 until i_part._2 - i_part._1)
        for (j <- 0 until j_part._2 - j_part._1) {
          if (verbose){
            println(i+" "+j+"spep. min: " + new_matrix(i)(j)+" "+(input_matrix(i + i_part._1)(k) + input_matrix(k)(j + j_part._1)))
          }
          new_matrix(i)(j) = Math.min(new_matrix(i)(j), input_matrix(i + i_part._1)(k) + input_matrix(k)(j + j_part._1))
        }
      if (verbose)
        print_matrix(new_matrix)
      new_matrix


}
 */