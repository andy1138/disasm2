package com.scalalabs.disasm.input

import java.io.DataInputStream
import java.io.ByteArrayInputStream
import java.security.InvalidParameterException

/** Load from byte array
 */
trait DebugLoader extends LoaderRepositoryComponent {
  type T  = String
  val loader = new DebugLoaderImpl[T]
  
  class DebugLoaderImpl[T] extends LoaderRepository[T] {
    var stream: DataInputStream = _ //new DataInputStream(new ByteArrayInputStream( Array[Byte]() ))

    var buffer:Array[Byte] = Array(0)
    
    
    def load(b:T) {
          println("DebugLoader a length: " + b.toString.length)
      b match {
        case a:String => 
          println("DebugLoader a length: " + a.length)
        	buffer  = a.sliding(2).map( i => Integer.parseInt( i, 16 ).toByte ).toArray
//        	buffer = x.flatten(_)
        	println("DebugLoader length: " + buffer.length)
		    	stream = new DataInputStream(new ByteArrayInputStream(buffer))

//          stream = new DataInputStream(new CharArrayInputStream(a))
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
