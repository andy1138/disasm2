package com.scalalabs.disasm.input

object Hello {

  val hello = "Hello World"
    
  def main( args: Array[String]) {
    println(hello)
  }
  
  def loopTest {
    while( true ) {
      println("loop")
    }
  }
  
  
  val inc = (a:Int, b:Int) => a+b
  
  
  def t3a() {
    t3(inc)
    
  }
  def t3( f:(Int, Int)=>Int ) {
    val r = f(1,2)
  }
  
  def expTest() {
    try {
      val i = System.currentTimeMillis() / 8
    } catch {
      case e:Exception => println("Help")
    }
  }
}