/**
 * MUT - Minimal Unit Test for Scala.
 *
 * Your test case must extend this class.
 *
 * Don't worry about license, do whatever you want with the source code C:
 *
 * === Very important - Definition of "test step" ===
 * Break down of a test case. A step is a block of code in test case which could
 * cause a test case to fail.
 */
class MUT {
  final class MutResult(val name: String, val successful: Boolean,
    set: => Unit, clean: => Unit) extends MUT {
    override def setup = set
    override def cleanup = clean
    /**
     * When a test step fails, "otherwise" code block executes.
     * This is not compulsory. Test step can have no "otherwise" block, however
     * you may want to do something when a test step fails.
     */
    def otherwise(block: => Unit) {
      if (!successful)
        block
    }
  }

  /**
   * Your test case class can override this method to define your own test setup
   * procedure.
   */
  def setup {}

  /**
   * Your test case class can override this method to define your own test clean
   * up procedure (e.g. closing files, shutdown sockets, etc).
   */
  def cleanup {}

  /**
   * Reset everything and execute the first test step.
   */
  def test(name: String)(firstStep: => Boolean): MutResult = {
    continue = true
    cleanup
    setup
    val successful = firstStep
    report(name, successful)
    new MutResult(name, successful, setup, cleanup)
  }

  /**
   * Almost identical to "test", but also prints the time spent on the in
   * milliseconds.
   */
  def timeTest(name: String)(firstStep: => Boolean): MutResult = {
    val begin = System.currentTimeMillis()
    val result = test(name)(firstStep)
    reportTime(name, System.currentTimeMillis() - begin)
    result
  }

  /**
   * Continue to run the next test step, without resetting resources.
   */
  def next(name: String)(nextStep: => Boolean): MutResult = {
    var successful = true
    if (continue) {
      successful = nextStep
      report(name, successful)
    }
    new MutResult(name, successful, setup, cleanup)
  }

  /**
   * Almost identical to "test", but also prints the time spent on the in
   * milliseconds.
   */
  def timeNext(name: String)(nextStep: => Boolean): MutResult = {
    val begin = System.currentTimeMillis()
    val result = next(name)(nextStep)
    reportTime(name, System.currentTimeMillis() - begin)
    result
  }

  /**
   * Prevent subsequent test steps from executing (due to serious runtime error
   * which invalidates the subsequent steps, such as network/disk failure).
   */
  def halt(implicit reason: String = "") {
    cleanup
    if (reason != "")
      println(reason)
    continue = false
  }

  private def report(name: String, ok: Boolean) {
    if (ok)
      println("===OK=== " + name)
    else
      println("==FAIL== " + name)
  }

  private def reportTime(name: String, time: Long) {
    println(name + " took " + time + "ms")
  }

  private var continue: Boolean = false
}
