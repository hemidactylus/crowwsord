/*
  CoinTotalEnvironment.scala
*/

import net.salamandrina.crowwsord._

//
object CoinTotalEnvironment extends PuzzleEnvironment {
  val puzzleName = "CoinTotal"
  type Configuration = CoinTotalConfiguration
  type PuzzleShape = CoinTotalPuzzleShape
  type ExtensionStep = CoinTotalExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Seq.empty)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    new Configuration(other.ctShape, other.ctContents :+ extension.newCoin)
  }
  //
  case class CoinTotalPuzzleShape(amount: Int, denominations: Set[Int]) extends AbstractPuzzleShape
  class CoinTotalConfiguration(shape: CoinTotalPuzzleShape, contents: Seq[Int]) extends AbstractConfiguration {
    private def getMinOfTwo(x: Int, y: Int): Int = if (x<y) x else y
    val ctShape: CoinTotalPuzzleShape = shape
    val ctContents: Seq[Int] = contents
    val minUsed: Int = {
      if(ctContents.isEmpty)
        ctShape.denominations.max
      else
        ctContents.min
    }
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean = {
      (ctContents :\ 0)(_ + _) <= ctShape.amount - extensionStep.newCoin
    }
    def stepProposals: Stream[ExtensionStep] = {
      (
        for(newCoin <- ctShape.denominations; if newCoin <= minUsed)
        yield new CoinTotalExtensionStep(newCoin)
      ).toStream
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = (ctContents :\ 0)(_ + _) == ctShape.amount
    override def toString: String = {
      val strDesc: String = contents.mkString(" + ")
      s"${puzzleName}<${ctShape.amount} = ${strDesc}>"
    }
  }
  class CoinTotalExtensionStep(val newCoin: Int) extends AbstractExtensionStep
}
