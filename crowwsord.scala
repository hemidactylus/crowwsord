/*
  crowwsord.scala

  main code
*/

import net.salamandrina.crowwsord.{CharLine,CharLineShape,CharLineContents}
import net.salamandrina.crowwsord.{IntLine,IntLineShape,IntLineContents}
import net.salamandrina.crowwsord.{SteppableIntLine,SteppableIntLineShape,SteppableIntLineContents}
import net.salamandrina.crowwsord.{CoinTotal,CoinTotalShape,CoinTotalContents}
import net.salamandrina.crowwsord.{SquareStepper,SquareStepperShape,SquareStepperContents}

/*
  TODO:
    remove displayFiller and generalise the toString
*/

object Crowwsord extends App {

  println("\nCharLine")
  val pieces: Set[Char] = Set('a','b','c','z','d')
  val confLen: CharLineShape = new CharLineShape(4)
  val confCont: CharLineContents = new CharLineContents(Seq.empty)
  val zeroConf: CharLine = new CharLine(confLen,confCont,pieces)
  zeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nIntLine")
  val ipieces: Set[Int] = Set(1,2,3,4,5)
  val iconfLen: IntLineShape = new IntLineShape(3)
  val iconfCont: IntLineContents = new IntLineContents(Seq.empty)
  val izeroConf: IntLine = new IntLine(iconfLen,iconfCont,ipieces)
  izeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nSteppableIntLine")
  val sconfLen: SteppableIntLineShape = new SteppableIntLineShape(5)
  val sconfCont: SteppableIntLineContents = new SteppableIntLineContents(Seq.empty)
  val szeroConf: SteppableIntLine = new SteppableIntLine(sconfLen,sconfCont)
  szeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nCoinTotal")
  val cpieces: Set[Int] = Set(10,5,2,1)
  val cconfLen: CoinTotalShape = new CoinTotalShape(11)
  val cconfCont: CoinTotalContents = new CoinTotalContents(Seq.empty)
  val czeroConf: CoinTotal = new CoinTotal(cconfLen,cconfCont,cpieces)
  czeroConf.findSolutions foreach ( c => println(s"  ${c}") )

  println("\nSquareStepper")
  val ssconfLen: SquareStepperShape = new SquareStepperShape(5, "100/4")
  val ssconfCont: SquareStepperContents = new SquareStepperContents(Seq.empty)
  val sszeroConf: SquareStepper = new SquareStepper(ssconfLen,ssconfCont)
  // sszeroConf.findSolutions foreach ( c => println(s"\n${c}\n") )

  val oneSol = (sszeroConf.findSolutions).head
  println(oneSol)
}
