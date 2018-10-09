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

  println("[Crowwsord] Starting\n")

  ( charLine.findSolutions.zipWithIndex) foreach ( { case (c,i) => println(s"${i}:   ${c}") } )

}
