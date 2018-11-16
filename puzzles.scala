/*
  puzzles.scala
    Main code to demonstrate the general puzzle-solvers
*/

import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}
import CoinTotalEnvironment.{CoinTotalConfiguration,CoinTotalPuzzleShape}
import SquareStepperEnvironment.{SquareStepperConfiguration,SquareStepperPuzzleShape,FillingStrategyEnum}

object PuzzleDemo extends App {

  // DEMO 1: charLine
  val charLinePuzzleShape=new CharLinePuzzleShape(7,Set('a','b','c','d','f','q','r','y','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeNewConfig(
    charLinePuzzleShape
  )
  println("\n[PuzzleDemo] Starting (charLine) ...")
  ( charLine.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )

  // DEMO 2: coinTotal
  val coinTotalPuzzleShape=new CoinTotalPuzzleShape(12,Set(1,2,5))
  val coinTotal: CoinTotalConfiguration = CoinTotalEnvironment.makeNewConfig(
    coinTotalPuzzleShape
  )
  println("\n[PuzzleDemo] Starting (coinTotal) ...")
  ( coinTotal.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )

  // DEMO 3A: squareStepper, quadruplicated
  val squareStepperPuzzleShape: SquareStepperPuzzleShape = new SquareStepperPuzzleShape(
    (10,10),
    FillingStrategyEnum.Quadruplicate
  )
  val squareStepper: SquareStepperConfiguration = SquareStepperEnvironment.makeNewConfig(
    squareStepperPuzzleShape
  )
  println("\n[PuzzleDemo] Starting (squareStepper,quadruplicated) ...")
  ( squareStepper.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )

}
