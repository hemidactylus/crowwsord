/*
  crowwsord.scala

  main code
*/

import net.salamandrina.crowwsord.{CharLine,CharLineShape}
import net.salamandrina.crowwsord.{IntLine,IntLineShape}
import net.salamandrina.crowwsord.{SteppableIntLine,SteppableIntLineShape}

object Crowwsord extends App {

  println("\nCharLine")
  val pieces: Set[Char] = Set('a','b','c','z','d')
  val confLen: CharLineShape = new CharLineShape(4)
  val zeroConf: CharLine = new CharLine(confLen,Seq.empty,pieces)
  zeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nIntLine")
  val ipieces: Set[Int] = Set(1,2,3,4,5)
  val iconfLen: IntLineShape = new IntLineShape(3)
  val izeroConf: IntLine = new IntLine(iconfLen,Seq.empty,ipieces)
  izeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nSteppableIntLine")
  val sconfLen: SteppableIntLineShape = new SteppableIntLineShape(5)
  val szeroConf: SteppableIntLine = new SteppableIntLine(sconfLen,Seq.empty)
  szeroConf.findSolutions foreach ( c => println(s"  ${c}") )

}
