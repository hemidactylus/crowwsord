/*
  crowwsord.scala
  main code
*/

object Crowwsord extends App {

  import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}

  val charLinePuzzleShape=new CharLinePuzzleShape(7,Set('a','b','c','d','f','q','r','y','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeConfig(
    charLinePuzzleShape
  )

  println("\n[Crowwsord] Starting ...")

  ( charLine.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"    ${i}:   ${c}") } )

  //

  import CoinTotalEnvironment.{CoinTotalConfiguration,CoinTotalPuzzleShape}

  val coinTotalPuzzleShape=new CoinTotalPuzzleShape(12,Set(1,2,5))
  val coinTotal: CoinTotalConfiguration = CoinTotalEnvironment.makeConfig(
    coinTotalPuzzleShape
  )

  println("\n[Crowwsord] Starting ...")

  ( coinTotal.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"    ${i}:   ${c}") } )

}
