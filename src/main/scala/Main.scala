import MyUtils._

import Array._
object Main {
  val nWorkers = 4
  def print_matrix(matrix: Array[Array[Int]]) = {
    for (i <- matrix.indices) {
      for ( j <- matrix.indices) {
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
      println("Step "+(k+1))

      for (i <- input_matrix.indices) {
        for (j <- input_matrix.indices) {
          new_matrix(i)(j) = Math.min(matrix(i)(j), matrix(i)(k) + matrix(k)(j))
        }
      }
      print_matrix(new_matrix)
      matrix = new_matrix
    }
    new_matrix
  }
  def compute_shortest_path_chunk(row: Array[Array[Int]], column: Array[Array[Int]], start: Int, end: Int, vertices: Int): Array[Array[Int]] = {
    val new_matrix = ofDim[Int](vertices, vertices)
    var matrix = column.slice(start, end)
    for (k <- 0 until vertices) {
      println("Step "+(k+1))

      for (i <- row.indices) {
        for (j <- column.indices) {
          new_matrix(i)(j) = Math.min(matrix(i)(j), row(i)(k) + column(k)(j))
        }
      }
      print_matrix(new_matrix)
      matrix = new_matrix
    }
    new_matrix
  }
  def compute_shortest_path_par(input_matrix: Array[Array[Int]], vertices: Int, nWorkers: Int): Array[Array[Int]] = {
    val step = (vertices / Math.sqrt(nWorkers)).toInt + 1
    println("Step = " + step)
    var i_part = (0, step)
    var j_part = (0, step)
    var last_step_add = false
    var j_part_last = (0,0)
    var chunks = List[Array[Array[Int]]]()
    print_matrix(input_matrix.slice(i_part._1 , i_part._2))
    println(i_part._1 + step , " ", Math.min(i_part._2 + step, vertices - 1))
    while(i_part._2 < vertices)
      while(j_part._2 < vertices)
        if (last_step_add) {
          val (chunk1, chunk2) = parallel(
            compute_shortest_path_chunk(
              input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2),
              input_matrix.slice(i_part._1 , i_part._2).slice(0, input_matrix.length),
              i_part._1 , i_part._2,
              i_part._2 - i_part._1),
            compute_shortest_path_chunk(
              input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2),
              input_matrix.slice(i_part._1 + step, Math.min(i_part._2 + step, vertices - 1)).slice(0, input_matrix.length),
              i_part._1 , Math.min(i_part._2 + step, vertices - 1),
              Math.min(i_part._2 + step, vertices - 1) - i_part._1)
          )

            //input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2))
              //input_matrix.slice(i_part._1 + step , Math.min(i_part._2 + step, vertices - 1)).slice(j_part._1 , j_part._2), Math.min(i_part._2 + step, vertices - 1) - i_part._1)

          chunks = chunk1 :: chunks
          chunks = chunk2 :: chunks
        } else {
          if (j_part._2 == vertices - 1){
            last_step_add = true
            j_part_last = (j_part._1, j_part._2)
          }
          else {
            val (chunk1, chunk2) = parallel(
              compute_shortest_path_basic(input_matrix.slice(i_part._1 , i_part._2).slice(j_part._1 , j_part._2), i_part._2 - i_part._1),
              compute_shortest_path_basic(input_matrix.slice(i_part._1 + step , Math.min(i_part._2 + step, vertices - 1)).slice(j_part._1 , j_part._2), Math.min(i_part._2 + step, vertices - 1) - i_part._1 - step)
            )
            chunks = chunk1 :: chunks
            chunks = chunk2 :: chunks
          }

        }
    for ( chunk <- chunks){
      print_matrix(chunk)
    }
    chunks(0)


  }
  def main(args: Array[String]): Unit = {
    val (vertices, points) =  readFromFile("input.txt")

    //println(vertices)
    val starting_matrix = compute_start_matr(points, vertices)
    println("Starting matrix: ")
    print_matrix(starting_matrix)
    val path_matrix = compute_shortest_path_basic(starting_matrix, vertices)
    println("Output: ")
    print_matrix(path_matrix)
    println((vertices / Math.sqrt(nWorkers)).toInt)
    val path_matrix_par = compute_shortest_path_par(starting_matrix, vertices, nWorkers)
  }
}
