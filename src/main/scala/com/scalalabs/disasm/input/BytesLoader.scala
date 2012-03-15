package com.scalalabs.disasm.input

import java.io.DataInputStream
import java.io.ByteArrayInputStream
import java.security.InvalidParameterException

/** Load from byte array
 */
trait BytesLoader extends LoaderRepositoryComponent {
  type T  = Array[Byte]
  val loader = new BytesLoaderImpl[T]
  
  class BytesLoaderImpl[T] extends LoaderRepository[T] {
    var stream: DataInputStream = _ //new DataInputStream(new ByteArrayInputStream( Array[Byte]() ))

    def load(b:T) {
      b match {
        case a:Array[Byte] => 
          stream = new DataInputStream(new ByteArrayInputStream(a))
          println("Setting Stream: " + stream)
        case _ => new InvalidParameterException("" + b)
      }
    }
    def nextByte:Int = stream.readByte & 0xff
		def nextU1:Byte = stream.readByte
		def nextU2:Int = stream.readChar.toInt
		def nextU4:Int = stream.readInt
		def nextLong:Long = stream.readLong

  }
} 
