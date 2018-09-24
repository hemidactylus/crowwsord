/*
  crowwsord.scala

  main code
*/

import net.salamandrina.crowwsord.{CharLine,IntLine,CharLineShape,IntLineShape}

object Crowwsord extends App {

  val pieces: Set[Char] = Set('a','b','c','z','d')
  val confLen: CharLineShape = new CharLineShape(4)
  val zeroConf: CharLine = new CharLine(confLen,Seq.empty,pieces)

  zeroConf.findSolutions foreach ( println(_) )

  val ipieces: Set[Int] = Set(1,2,3,4,5)
  val iconfLen: IntLineShape = new IntLineShape(3)
  val izeroConf: IntLine = new IntLine(iconfLen,Seq.empty,ipieces)

  izeroConf.findSolutions foreach ( println(_) )

}
