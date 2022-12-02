package com.github.skac112.klee.paintrunners

import cats.Monad

/**
  * Painr runner which may be interrupted by user.
  */
abstract class InterruptibleRunner[M[_]: Monad] extends PaintRunner {
  var end = false
  val runner = this

  def paints: Seq[() => M[Unit]]

  val t1 = new Thread(new Runnable {
    def run: Unit = {
      scala.io.StdIn.readChar()
      println("Runner will be interrupted soon...")
      runner.synchronized(
        runner.end = true
      )
    }
  })

   t1.start
   var i = 0
   do {
     println(s"Running No. $i")
     paints(i).apply()
     i = i + 1
   } while (synchronized(!end))
}
