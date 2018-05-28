import MyUtils._

import Array._
object Main {

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

  def compute_shortest_path_brute(graph: List[(Int, Int, Int)], input_matrix: Array[Array[Int]], vertices: Int): Array[Array[Int]] = {
    val new_matrix = ofDim[Int](vertices, vertices)
    var matrix = input_matrix
    for (k <- 0 until vertices) {
      println("Step "+(k+1))

      for (i <- 0 until vertices) {
        for (j <- 0 until vertices) {
          new_matrix(i)(j) = Math.min(matrix(i)(j), matrix(i)(k) + matrix(k)(j))
        }
      }
      print_matrix(new_matrix)
      matrix = new_matrix
    }
    new_matrix
  }

  def main(args: Array[String]): Unit = {
    val (vertices, points) =  readFromFile("input.txt")

    //println(vertices)
    val starting_matrix = compute_start_matr(points, vertices)
    println("Starting matrix: ")
    print_matrix(starting_matrix)
    val path_matrix = compute_shortest_path_brute(points, starting_matrix, vertices)
    println("Output: ")
    print_matrix(path_matrix)
  }
}
