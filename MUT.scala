// Your package definition goes here, e.g.
// package com.example.project.test

/**
 * MUT - A more interesting way to test your Scala classes.
 *
 * == Idea ==
 * Using MUT, rather than writing specifications to test a class, you write
 * specifications to test how one (or a few) class instance(s) behave in
 * different states.
 *
 * == Why use MUT ==
 * Suppose that you have a class which has:
 * - Operations (change the state of class)
 * - Specifications (documented behaviors of class in different states)
 * And the state of class is determined by the value of the class attributes.
 * There are two specifications which from two perspectives define the behavior
 * of an operation that transforms the class instance's state from S1 to S2
 * then to S3.
 * Our goal is to verify the correctness of the class instance in each of the
 * states against the two specifications.
 * Using other unit testing tool, you may need to write four tests:
 * Test 1 - setup an instance at state S1 and test against spec 1
 * Test 2 - setup an instance at state S1, execute the operation to change the
 * state to S2 then test against spec 1
 * Test 3 - setup an instance at state S1, execute the operation to
 * change to state to S2, execute again to chance to state S3 then test against
 * spec 1.
 * Test 4 - setup an instance at state S1 and test against spec 2
 *
 * MUT is thus designed to eliminate the need of the repetitive class instance
 * setup and states transition. The above three tests will be written as the
 * following MUT test:
 *
 * test("Test 1") { // instance now at state 1
 *   test against spec 1
 * }
 * next("Test 2") { // instance now at state 2
 *   execute the operation
 *   test against spec 1
 * }
 * next("Test 3") { // instance now at state 3
 *   execute the operation
 *   test against spec 1
 * }
 *
 * test("Test 4") { // instance now at state 1
 *   test against spec 2
 * }
 * cleanup
 *
 * == How To Use MUT ==
 * class MyMutTest extends MUT {
 *   var instance: MyClass = null
 *   override def setup {
 *     // Setup your instance(s)
 *     instance = new MyClass
 *   }
 *   override def cleanup {
 *     // Clean up for the whole test
 *     if (instance != null) // You must have this line
 *       instance.close().deleteTempFile()
 *   }
 *
 *   test("Spec1 - instance should behave like A") {
 *     instance.operation(123)
 *     if (instance state is like A)
 *       true
 *     else
 *       false
 *   } otherwise {
 *     // Help to diagnose the problem when the preceding block falsed
 *     println("instance has the incorrect state because of GGG")
 *     // Skip all the following "next(){}" tests
 *     halt("Very serious error!! Shall not proceed to the next state tests!")
 *   }
 *   next("Sepc1 - instance should behave like B") {
 *     instance.operation(234)
 *     if (instance state is like B)
 *       true
 *     else
 *       false
 *   } otherwise { println("instance has the incorrect state") }
 *   // Following "test(){}", You can have any number of "next(){}" tests
 *   // "otherwise{}" block is not compulsory
 *   next("...") { ... }
 *
 *   // Call test(){} will cleanup then re-setup
 *   test("Spec2 - instance should behave like C") {
 *     ...
 *   }
 *
 *   // You can also use "timeTest" and "timeNext" instead of "test" and "next"
 *   // to print out the time taken to complete the test.
 *
 *   cleanup // Do not forget this line
 * }
 *
 * == Author ==
 * Houzuo (Howard) Guo
 * http://twitter.com/#!/hzguo
 * http://www.linkedin.com/pub/houzuo-guo/27/b62/2b1
 * https://github.com/HouzuoGuo
 *
 * == License ==
 * Use the source code in anyway you want, but please retain the author
 * information, in case the user of your test code needs technical support.
 */
class MUT {
  final class MutResult(val name: String, val successful: Boolean,
    set: => Unit, clean: => Unit) extends MUT {
    override def setup = set
    override def cleanup = clean
    def otherwise(block: => Unit) {
      if (!successful)
        block
    }
  }

  def setup {}
  def cleanup {}

  def test(name: String)(firstStep: => Boolean): MutResult = {
    continue = true
    cleanup
    setup
    val successful = firstStep
    report(name, successful)
    new MutResult(name, successful, setup, cleanup)
  }

  def timeTest(name: String)(firstStep: => Boolean): MutResult = {
    val begin = System.nanoTime()
    val result = test(name)(firstStep)
    reportTime(name, System.nanoTime() - begin)
    result
  }

  def next(name: String)(nextStep: => Boolean): MutResult = {
    var successful = true
    if (continue) {
      successful = nextStep
      report(name, successful)
    }
    new MutResult(name, successful, setup, cleanup)
  }

  def timeNext(name: String)(nextStep: => Boolean): MutResult = {
    val begin = System.nanoTime()
    val result = next(name)(nextStep)
    reportTime(name, System.nanoTime() - begin)
    result
  }

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
    println(name + " took " + time + "ns")
  }

  private var continue: Boolean = false
}
