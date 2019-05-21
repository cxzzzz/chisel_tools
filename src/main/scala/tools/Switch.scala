
package tools

import chisel3._

class switch[T <: Data](toMux: (T) => T) {

  def otherwise(alt: T): T = {
    toMux(alt)
  }

  def elseswitch(cond: Bool)(con: T): switch[T] = {

    def f(alt: T) = Mux(cond, con, alt)

    new switch((alt: T) => toMux(f(alt)))
  }
}

object switch {

  def apply[T <: Data](cond: Bool)(con: T): switch[T] = {
    new switch((alt: T) => Mux(cond, con, alt))
  }
}



