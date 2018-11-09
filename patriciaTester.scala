/*
  patriciaTester.scala
*/

object PTTester extends App {
  import scala.io.Source.fromFile
  val wds: Set[String] = fromFile("FULL_WORDS.txt").getLines().filter(_ != "").map( _.toUpperCase ).toSet
  val pt: PatriciaTreeSet = PatriciaTreeSet(wds)

  def lenplus(s:String) = (
    s.map( 1000000+_.toInt )
  ).sum

  val winnerWord:String=(pt :\ "") (
    { case (w1:String,w2:String) => if(lenplus(w1)>lenplus(w2)) w1 else w2}
  )

  println(s"Winner word <${winnerWord}>")
  val wPrefix:String="PERN"
  println(s"Prefix = <${wPrefix}>")

  println(pt.withPrefix(wPrefix))

  println(pt.withPrefix(wPrefix).suffixes.keys)
  println(if (pt.withPrefix(wPrefix).value) "also <close>" else "")

  val msk: List[Set[Char]]=List(
    Set('P','A'),
    Set('O','L','E'),
    Set('R','I','T')
  )
  println(pt.maskedIterator(msk).mkString("<","|",">"))

  /* we pretend we have a config such as
        REALE
        ALFIN
        ?....#
     and need to enumerate possible words on ?->
  */
  val wordStarts: List[String]=List("RA","EL","AF","LI","EN")
  val mask: List[Set[Char]]=wordStarts.map(
    pt.withPrefix(_).suffixes.keys.toSet
  )
  println(
    pt
      .maskedIterator(mask)
      .map( "REALE\nALFIN\n"+_+"#" )
      .mkString("\n\n")
  )

}
