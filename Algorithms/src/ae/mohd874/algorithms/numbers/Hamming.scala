package ae.mohd874.algorithms.numbers

/*
 * This code was copied and modified from another source. 
 * The original source can be found below. 
 * 
 * Code original source: http://rosettacode.org/wiki/Hamming_numbers#Scala
 */
object Hamming {

  def apply(): Stream[BigInt] = {
	hamming
  }
  
  def hamming: Stream[BigInt] = {
    def merge(inx: Stream[BigInt], iny: Stream[BigInt]): Stream[BigInt] = {
      if (inx.head < iny.head) inx.head #:: merge(inx.tail, iny) else if (iny.head < inx.head) iny.head #:: merge(inx, iny.tail) else
        merge(inx, iny.tail)
    }

    1 #:: merge(hamming map (_ * 2), merge(hamming map (_ * 3), hamming map (_ * 5)))
  }
}