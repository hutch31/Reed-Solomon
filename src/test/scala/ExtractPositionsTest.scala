package Rs
import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class ExtractPositionsTest extends AnyFreeSpec with ChiselScalatestTester {
  //behavior of "ExtractPositions"

  "correctly extract positions of high bits" in {
    // Parameters for the test
    val N = 8
    val M = 4
    val pipelineInterval = 2

    test(new ExtractPositions(N, M, pipelineInterval)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      implicit val clk = dut.clock

      // Define test cases: (input vector, expected positions, expected tkeep)
      val testCases = Seq(
        (0xC2.U, Seq(1, 6, 7), Seq(true, true, true, false)), // inVec = 0xC2
        (0xDD.U, Seq(0, 2, 3, 4), Seq(true, true, true, true)), // inVec = 0xDD
        (0x00.U, Seq(0, 0, 0, 0), Seq(false, false, false, false)), // inVec = 0x00
        (0xFF.U, Seq(0, 1, 2, 3), Seq(true, true, true, true)) // inVec = 0xFF (only first M positions)
      )

      // Pipeline latency
      val pipelineLatency = ((N + pipelineInterval - 1) / pipelineInterval)

      // Initialize a queue to keep track of expected outputs
      val expectedOutputs = scala.collection.mutable.Queue.empty[(Seq[Int], Seq[Boolean])]

      // Apply test cases
      for ((inVec, expectedPositions, expectedTkeep) <- testCases) {
        // Apply input
        dut.io.in.bits.poke(inVec)
        dut.io.in.valid.poke(1)
        clk.step()

        // Record expected outputs after pipeline latency
        expectedOutputs.enqueue((expectedPositions, expectedTkeep))
      }
      dut.io.in.valid.poke(0)
      // Continue clocking to observe outputs
      for (_ <- 0 until pipelineLatency + testCases.length) {
        if (dut.io.outValid.peek().litToBoolean) {
          val positions = dut.io.positions.map(_.peek().litValue.toInt)
          val tkeep = dut.io.outTkeep.map(_.peek().litToBoolean)
          val (expectedPositions, expectedTkeep) = expectedOutputs.dequeue()

          // Assertions
          assert(positions.zip(expectedPositions).forall { case (a, b) => a == b },
            s"Positions mismatch. Got $positions, expected $expectedPositions")
          assert(tkeep.zip(expectedTkeep).forall { case (a, b) => a == b },
            s"tkeep mismatch. Got $tkeep, expected $expectedTkeep")
        }
        clk.step()
      }
    }
  }
}
