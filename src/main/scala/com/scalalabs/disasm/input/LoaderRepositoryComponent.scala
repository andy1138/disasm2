package com.scalalabs.disasm.input

import java.io.DataInputStream
import java.io.ByteArrayInputStream

trait LoaderRepositoryComponent {
  type T
  def loader: LoaderRepository[T]
  
  trait LoaderRepository[T] {
    def load(x:T) 
    
//    var stream: DataInputStream = _ //new DataInputStream(new ByteArrayInputStream( Array[Byte]() ))
//
    def nextByte:Int //= stream.readByte
		def nextU1:Byte 
		def nextU2:Int //= stream.readChar
		def nextU4:Int //= stream.readInt
		def nextLong:Long //= stream.readLong

  }
}
