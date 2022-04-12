package test

import ap.SimpleAPI
import ap.parser._

object Sudoku {
  def main(args: Array[String]) =
    SimpleAPI.withProver { p =>
      import p._
      import IExpression._

      println("Solving Sudoku ...")

      setConstructProofs(true)

      // declare 9x9 variables, each ranging from 1 to 9
      val rows = for (row <- 0 until 9) yield createConstants(9, 1 to 9)

      !! (and(rows map distinct))
      !! (and(for (col <- 0 until 9)
        yield distinct(for (row <- 0 until 9) yield rows(row)(col))))

      !! (and(for (rblock <- 0 until 3; cblock <- 0 until 3)
        yield distinct(for (row <- 0 until 3; col <- 0 until 3)
          yield rows(rblock*3 + row)(cblock*3 + col))))

      // Add constraints for initial elements

      !! (rows(0)(0) === 7)
      !! (rows(0)(2) === 4)
      !! (rows(1)(0) === 9)
      !! (rows(1)(1) === 1)

      !! (rows(1)(5) === 7)

      !! (rows(1)(6) === 6)
      !! (rows(1)(7) === 8)
      !! (rows(0)(8) === 5)

      !! (rows(3)(1) === 3)
      !! (rows(5)(1) === 4)

      !! (rows(3)(4) === 2)
      !! (rows(4)(3) === 3)
      //      !! (rows(4)(5) === 9)    // uncomment this line to make problem unsat
      !! (rows(5)(4) === 8)

      !! (rows(3)(7) === 5)
      !! (rows(5)(6) === 3)

      !! (rows(6)(2) === 2)
      !! (rows(8)(2) === 9)

      println(???)

      ??? match {

        case SimpleAPI.ProverStatus.Sat =>
          for (row <- rows)
            println((row map eval) mkString " ")

        case SimpleAPI.ProverStatus.Unsat =>
          println(certificateAsString(Map(),
            ap.parameters.Param.InputFormat.Princess))
      }
    }
}