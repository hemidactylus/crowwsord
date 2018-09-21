/*
  crowwsord.scala

  main code
*/

import net.salamandrina.crowwsord.Configuration

object Crowwsord extends App {

  val pieces: Set[Char] = Set('a','b','c','z','d')
  val confLen: Int = 4
  val zeroConf: Configuration = new Configuration(confLen,Seq.empty,pieces)

  zeroConf.findSolutions foreach ( println(_) )

}
