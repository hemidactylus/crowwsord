/*
  crow2.scala

  main code
*/

import net.salamandrina.crowwsord.{CharLineEnvironment}

object Crow2 extends App {
  import CharLineEnvironment.{CharLineConfiguration,CharLinePuzzleShape}

  val charLinePuzzleShape=new CharLinePuzzleShape(7,Set('a','b','c','d','f','q','r','y','z'))
  val charLine: CharLineConfiguration = CharLineEnvironment.makeConfig(
    charLinePuzzleShape
  )

  println("Hi.")

  ( charLine.findSolutions zipWithIndex) foreach ( { case (c,i) => println(s"${i}:   ${c}") } )

}
