import MyUtils._

import Array._
object Main {
  val nWorkers = 4 // power of 2
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

//  def compute_shortest_path_chunk(rows: Array[Array[Int]],
//                                  columns: Array[Array[Int]],
//                                  i_part: (Int, Int),
//                                  j_part: (Int, Int)
//                                  //vertices: Int
//                                 ): Array[Array[Int]] = {
//    println("i part: " + i_part)
//    println("j part: " + j_part)
//    println("Rows:")
//    print_matrix(rows)
//    val new_matrix = ofDim[Int](i_part._2 - i_part._1, j_part._2 - j_part._1)
//    for (i <- 0 until i_part._2 - i_part._1)
//      for (j <- 0 until j_part._2 - j_part._1)
//        new_matrix(i)(j) = rows(i_part._1 + i)(j_part._1 + j)
//    var trows = rows
//    var tcolumns = columns
//    for (k <- rows.indices) {
//      println("Step "+(k+1))
//
//      for (i <- 0 until i_part._2 - i_part._1) {
//        for (j <- 0 until j_part._2 - j_part._1) {
//          new_matrix(i)(j) = Math.min(new_matrix(i)(j), trows(i)(k) + tcolumns(k)(j))
//        }
//      }
//      for (i <- 0 until i_part._2 - i_part._1)
//        for (j <- 0 until j_part._2 - j_part._1) {
//          trows(i)(j_part._1 + j) = new_matrix(i)(j)
//          tcolumns(i_part._1 + i)(j) = new_matrix(i)(j)
//        }
//      print_matrix(new_matrix)
//
//    }
//    new_matrix
//  }
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
  def parallel_step(input_matrix: Array[Array[Int]], nWorkers: Int, k: Int): Array[Array[Int]] = {
    if (nWorkers == 2) {
      val (up, down) = parallel(compute_shortest_path_chunk(matrix, (0, vertices / 2), (0, vertices), verbose = false, k),
        compute_shortest_path_chunk(matrix, (vertices / 2, vertices), (0, vertices), false, k))
      matrix = up ++ down
    } else
    if (nWorkers == 4){
      val (up1, up2, down1, down2) = parallel(
        compute_shortest_path_chunk(matrix, (0, vertices / 2), (0, vertices/2), verbose = false, k),
        compute_shortest_path_chunk(matrix, (0, vertices / 2), (vertices/2, vertices), verbose = false, k),
        compute_shortest_path_chunk(matrix, (vertices/2, vertices), (0, vertices/2), verbose = false, k),
        compute_shortest_path_chunk(matrix, (vertices / 2, vertices), (vertices/2, vertices), false, k)
      )
      matrix = combine(matrix, up1, up2, down1, down2)
    } else
    {

    }

  }
  def compute_shortest_path_par(input_matrix: Array[Array[Int]], vertices: Int, nWorkers: Int): Array[Array[Int]] = {
    var matrix = input_matrix
    for (k <- input_matrix.indices) {
      println("Step "+(k+1))
      matrix = parallel_step(input_matrix, nWorkers, k)
      print_matrix(matrix)
      println()
    }
    matrix
  }






//  def compute_shortest_path_chunk(rows: Array[Array[Int]],
//                                  columns: Array[Array[Int]],
//                                  i_part: (Int, Int),
//                                  j_part: (Int, Int)
//                                  //vertices: Int
//                                 ): Array[Array[Int]] = {
//    println("i part: " + i_part)
//    println("j part: " + j_part)
//    println("Rows:")
//    print_matrix(rows)
//    val new_matrix = ofDim[Int](i_part._2 - i_part._1, j_part._2 - j_part._1)
//    for (i <- 0 until i_part._2 - i_part._1)
//      for (j <- 0 until j_part._2 - j_part._1)
//        new_matrix(i)(j) = rows(i_part._1 + i)(j_part._1 + j)
//    var trows = rows
//    var tcolumns = columns
//    for (k <- rows.indices) {
//      println("Step "+(k+1))
//
//      for (i <- 0 until i_part._2 - i_part._1) {
//        for (j <- 0 until j_part._2 - j_part._1) {
//          new_matrix(i)(j) = Math.min(new_matrix(i)(j), trows(i)(k) + tcolumns(k)(j))
//        }
//      }
//      for (i <- 0 until i_part._2 - i_part._1)
//        for (j <- 0 until j_part._2 - j_part._1) {
//          trows(i)(j_part._1 + j) = new_matrix(i)(j)
//          tcolumns(i_part._1 + i)(j) = new_matrix(i)(j)
//        }
//      print_matrix(new_matrix)
//
//    }
//    new_matrix
//  }
//  def compute_shortest_path_par(input_matrix: Array[Array[Int]], vertices: Int, nWorkers: Int): Array[Array[Int]] = {
//    val step = (vertices / Math.sqrt(nWorkers)).toInt + 1
//    println("Step = " + step)
//    var i_part = (0, step)
//    var j_part = (0, step)
//    var last_step_add = false
//    var j_part_last = (0,0)
//    var chunks = List[Array[Array[Int]]]()
//    println("First chunk")
//    print_matrix(input_matrix.slice(i_part._1 , i_part._2))
//    print_matrix(get_columns(input_matrix, i_part._1 , i_part._2))
//    println("Second chunk")
//    //print_matrix(input_matrix.slice(j_part._1,  j_part._2))
//    print_matrix(get_columns(input_matrix, i_part._1 + step,  Math.min(i_part._2 + step, vertices)))
//    // println(i_part._1 + step , " ", Math.min(i_part._2 + step, vertices - 1))
//    while(i_part._2 < vertices)
//      while(j_part._2 < vertices)
//        if (last_step_add) {
//          //TO DO
//
//
////          val (chunk1, chunk2) = parallel(
////            compute_shortest_path_chunk(
////              input_matrix,
////              i_part, j_part,
////              i_part._2 - i_part._1),
////            compute_shortest_path_chunk(
////              input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2),
////              input_matrix.slice(i_part._1 + step, Math.min(i_part._2 + step, vertices - 1)).slice(0, input_matrix.length),
////              i_part._1 , Math.min(i_part._2 + step, vertices - 1),
////              Math.min(i_part._2 + step, vertices - 1) - i_part._1)
////          )
////
////            //input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2))
////              //input_matrix.slice(i_part._1 + step , Math.min(i_part._2 + step, vertices - 1)).slice(j_part._1 , j_part._2), Math.min(i_part._2 + step, vertices - 1) - i_part._1)
////
////          chunks = chunk1 :: chunks
////          chunks = chunk2 :: chunks
//        } else {
//          if (j_part._2 == vertices - 1){
//            last_step_add = true
//            j_part_last = (j_part._1, j_part._2)
//          }
//          else {
//            val (chunk1, chunk2) = parallel(
//              compute_shortest_path_chunk(
//                input_matrix.slice(i_part._1 , i_part._2),
//                get_columns(input_matrix, j_part._1 , j_part._2),
//                i_part , j_part
//                //,i_part._2 - i_part._1
//              ),
//              compute_shortest_path_chunk(
//                input_matrix.slice(j_part._1,  j_part._2),
//                get_columns(input_matrix, j_part._1 + step,  Math.min(j_part._2 + step, vertices)),
//                i_part, (j_part._1 + step, Math.min(j_part._2 + step, vertices ))
//                //,Math.min(i_part._2 + step, vertices) - i_part._1
//              )
//
//
//
////                input_matrix.slice(0, input_matrix.length).slice(j_part._1 , j_part._2),
////                input_matrix.slice(i_part._1 + step, Math.min(i_part._2 + step, vertices - 1)).slice(0, input_matrix.length),
////                i_part._1 , Math.min(i_part._2 + step, vertices - 1),
////                Math.min(i_part._2 + step, vertices - 1) - i_part._1)
//            )
//            chunks = chunk1 :: chunks
//            chunks = chunk2 :: chunks
//          }
//
//        }
//    for ( chunk <- chunks){
//      print_matrix(chunk)
//    }
//    chunks(0)
//
//
//  }
  def get_columns(input_matrix: Array[Array[Int]], start: Int, end: Int): Array[Array[Int]] ={
    println(start + " - " + end)
    val res_matrix = ofDim[Int](input_matrix.length, end - start)
    for (i <- input_matrix.indices) {
      for (j <- start until end) {
        res_matrix(i)(j - start) = input_matrix(i)(j)
      }
    }
    res_matrix
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
    print_matrix(path_matrix_par)
  }
}
