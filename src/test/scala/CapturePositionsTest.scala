package Rs

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.util._

class CapturePositionsTest extends AnyFreeSpec with ChiselScalatestTester {

  //behavior of "CapturePositions"
  "CapturePositionsTest" in {

    val projectRoot = System.getProperty("project.root")
    val c = JsonReader.readConfig(projectRoot + "/rs.json")

    test(new CapturePositions(c)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the DUT (Device Under Test)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      
      // Define some test values for bitPos
      val testPositions = Seq(0.U, 1.U, 2.U, 3.U, 0.U)

      // Iterate through cycles and provide bit positions
      for (i <- testPositions.indices) {
        dut.io.bitPos.valid.poke(true.B)
        dut.io.bitPos.last.poke(if (i == testPositions.size - 1) true.B else false.B)
        dut.io.bitPos.pos.poke(testPositions(i))

        dut.clock.step()
      }

      dut.io.bitPos.valid.poke(false.B)
      dut.io.bitPos.last.poke(false.B)
      // Wait for the last flag to propagate and capture the output
      dut.clock.step(50)

      // Check output
      dut.io.errPosIf.valid.expect(true.B)  // Assuming the last cycle generates a valid output
                                            // You could add more expectations based on the expected result.
    }
  }
}
