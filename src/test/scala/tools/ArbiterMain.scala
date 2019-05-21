
package tools

import chisel3._

object WeightedArbiterMain extends App{
  iotesters.Driver.execute(args,()=> new WeightedArbiterWithFifo(1)){
    c => new Base1WeightedArbiterTest(c)
  }
}
