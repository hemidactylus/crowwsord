/*
  crowwsord.scala
  main code
*/

object Crowwsord extends App {

  /*
  import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}

  val charLinePuzzleShape=new CharLinePuzzleShape(7,Set('a','b','c','d','f','q','r','y','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeConfig(
    charLinePuzzleShape
  )

  println("\n[Crowwsord] Starting (charLine) ...")

  ( charLine.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )
  */

  /*
  import CoinTotalEnvironment.{CoinTotalConfiguration,CoinTotalPuzzleShape}

  val coinTotalPuzzleShape=new CoinTotalPuzzleShape(12,Set(1,2,5))
  val coinTotal: CoinTotalConfiguration = CoinTotalEnvironment.makeConfig(
    coinTotalPuzzleShape
  )

  println("\n[Crowwsord] Starting (coinTotal) ...")

  ( coinTotal.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )
  */

  import SquareStepperEnvironment.{SquareStepperConfiguration,SquareStepperPuzzleShape,FillingStrategyEnum}

  val squareStepperPuzzleShape: SquareStepperPuzzleShape = new SquareStepperPuzzleShape(
    (5,5),
    FillingStrategyEnum.Monoplicate
  )
  val squareStepper: SquareStepperConfiguration = SquareStepperEnvironment.makeConfig(
    squareStepperPuzzleShape
  )

  println("\n[Crowwsord] Starting (squareStepper) ...")

  ( squareStepper.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"\n    Sol=${i+1}:   ${c}") } )

}
