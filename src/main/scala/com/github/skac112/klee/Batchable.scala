package com.github.skac112.klee

trait Batchable[T, U] extends ((T) => U) {
  def applyBatch(items: Seq[T]): Seq[U] = items map apply _
}
