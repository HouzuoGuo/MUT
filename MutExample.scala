class Overheating extends Exception

/**
 * This class is to be unit tested.
 */
class CPU {
  def fetch(): Boolean = false
  def decode(): Boolean = true
  def execute(): Boolean = { throw new Overheating }
  def writeback(): Boolean = true

  var core1 = "off"
  var core2 = "off"
}

/**
 * MUT test for CPU.
 */
class CpuMUT extends MUT {
  val cpu = new CPU

  override def setup {
    // turn CPU cores on
    cpu.core1 = "on"
    cpu.core2 = "on"
  }

  override def cleanup {
    // turn CPU cores off
    cpu.core1 = "off"
    cpu.core2 = "off"
  }

  test("Fetching instruction") {
    if (cpu.fetch())
      true
    else
      false
  } otherwise { println("CPU cannot fetch an instruction\n") }
  timeNext("Decode instruction") {
    if (cpu.decode())
      true
    else
      false
  } otherwise { println("CPU cannot decode an instruction\n") }
  timeNext("Execute instruction") {
    try {
      if (cpu.execute())
        true
      else
        false
    } catch {
      case e: Overheating =>
        {
          halt("\nCPU overheated!")
          println("CPU automatically shuts down")
        }
        false
    }
  } otherwise { println("CPU cannot execute an instruction\n") }
  next("Write result back into memory") {
    cpu.writeback()
  } otherwise { println("CPU cannot write result back into memory") }

  timeTest("CPU has been reset. Fetching instruction") {
    if (cpu.fetch())
      true
    else
      false
  } otherwise { println("CPU cannot fetch an instruction") }
  halt()
}

object MutExample {
  def main(args: Array[String]): Unit = { new CpuMUT() }
}