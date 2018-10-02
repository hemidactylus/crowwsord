/*
  crow2.scala

  main code
*/

import net.salamandrina.crowwsord.{CharLineEnvironment}

object Crow2 extends App {
  import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}

  val charLinePuzzleShape=new CharLinePuzzleShape(4,Set('a','b','c','d','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeConfig(
    charLinePuzzleShape
  )

  println("Hi.")

  charLine.findSolutions foreach ( c => println(s"  ${c}") )  

}
