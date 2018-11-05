/*
  CharLineEnvironment.scala
*/

import net.salamandrina.crowwsord._

object CharLineEnvironment extends PuzzleEnvironment {
  val puzzleName = "CharLine"
  type Configuration = CharLineConfiguration
  type PuzzleShape = CharLinePuzzleShape
  type ExtensionStep = CharLineExtensionStep
  def makeConfig(shape: PuzzleShape): Configuration = new Configuration(shape,Seq.empty)
  def makeConfig(other: Configuration, extension: ExtensionStep): Configuration = {
    new Configuration(other.clShape, other.clContents :+ extension.newChar)
  }
  //
  class CharLinePuzzleShape(len: Int, universe: Set[Char]) extends AbstractPuzzleShape {
    val clLen: Int = len
    val clUniverse: Set[Char] = universe
  }
  class CharLineConfiguration(shape: CharLinePuzzleShape, contents: Seq[Char]) extends AbstractConfiguration {
    val clShape: CharLinePuzzleShape = shape
    val clContents: Seq[Char] = contents
    //
    def extendWith(extensionStep: ExtensionStep): Configuration = makeConfig(this,extensionStep)
    def canExtendWith(extensionStep: ExtensionStep): Boolean =
      clContents.isEmpty || (clContents.last < extensionStep.newChar)
    def stepProposals: Stream[ExtensionStep] = {
      (
        for(newChar <- clShape.clUniverse; ext=new ExtensionStep(newChar); if canExtendWith(ext))
        yield ext
      ).toStream
    }
    def lastTouch: Configuration = this
    def isCompleted: Boolean = clContents.length == clShape.clLen
    override def toString: String = {
      val strDesc: String = contents.mkString("") + List.fill(shape.clLen-contents.length)('.').mkString("")
      s"${puzzleName}<${strDesc}>"
    }
  }
  class CharLineExtensionStep(val newChar: Char) extends AbstractExtensionStep
}
