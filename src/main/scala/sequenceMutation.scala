import scala.util.Random

object sequenceMutation {
  private val nucleotides: String = "ATGC"
  private val mutations: Map[String, String] = Map("I" -> "Insert", "D" -> "Delete", "S" -> "Substitute")
  private val mutationChance: String = "IDS"
  private val mutationCount: Integer = 10
  private val sequenceLength: Integer = 250
  private val rand: Random = new Random(System.currentTimeMillis())
  private val baseSequence: String = generateSequence

  def main(args: Array[String]): Unit = {
    val baseSequenceStat: Map[String, Int] = calcSequenceStats(this.baseSequence)
    val (mutatedSequence, mutationInfo): (String, Array[(String, Integer)]) = mutateSequence
    val mutatedSequenceStat: Map[String, Int] = calcSequenceStats(mutatedSequence)

    printSequence(this.baseSequence)
    printSequenceStats(baseSequenceStat)
    printMutationInfo(mutationInfo)
    printSequence(mutatedSequence)
    printSequenceStats(mutatedSequenceStat)
  }

  private def generateSequence: String = {
    for (i <- 0 until this.sequenceLength)
      yield this.nucleotides(this.rand.nextInt(this.nucleotides.length))
  }.mkString

  private def calcSequenceStats(seq: String): Map[String, Int] = this.nucleotides
    .map(nucleotide => (nucleotide.toString, seq.count(_ == nucleotide)))
    .toMap

  private def printSequence(seq: String, size: Integer = 50): Unit = {
    println("SEQUENCE:")
    seq.grouped(size).foreach(println)
  }

  private def printSequenceStats(seqStats: Map[String, Int]): Unit = {
    println("SEQUENCE STATISTICS:")
    seqStats.foreach { case (k, v) => println(s"$k = $v") }
    println(s"Total = ${seqStats.values.sum}")
  }

  private def printMutationInfo(mutInfo: Array[(String, Integer)]): Unit = {
    println("MUTATIONS:")
    mutInfo.foreach { case (k, v) => println(s"$k -> $v") }
  }

  private def mutateSequence: (String, Array[(String, Integer)]) = {
    var mutatedSeq: String = this.baseSequence
    var mutInfo: Array[(String, Integer)] = Array()
    for (i <- 0 until this.mutationCount) {
      val mutType: String = this.mutationChance(this.rand.nextInt(this.mutationChance.length)).toString
      val pos: Integer = this.rand.nextInt(mutatedSeq.length)
      mutatedSeq = mutType match
        case "I" => mutatedSeq.take(pos) + this.nucleotides(this.rand.nextInt(this.nucleotides.length)).toString + mutatedSeq.drop(pos)
        case "D" => mutatedSeq.take(pos) + mutatedSeq.drop(pos + 1)
        case "S" => mutatedSeq.take(pos) + this.nucleotides(this.rand.nextInt(this.nucleotides.length)).toString + mutatedSeq.drop(pos + 1)
      mutInfo = mutInfo :+ (this.mutations(mutType) -> pos)
    }
    (mutatedSeq, mutInfo)
  }
}
