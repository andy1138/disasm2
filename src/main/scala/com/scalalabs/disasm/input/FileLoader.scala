package com.scalalabs.disasm.input

import java.io.DataInputStream
//import java.io.ByteArrayInputStream
import java.io.FileInputStream

/** Load from file
 */
trait FileLoader extends LoaderRepositoryComponent {
  type T = String
  val loader = new FileLoaderImpl[T]
  
  class FileLoaderImpl[T] extends LoaderRepository[T] {
    var stream: DataInputStream = _ 
    var count = 0
    
    def load(s:T) {
      s match {
        case filename:String => 
          val file = new java.io.File( filename  )
          println(">>FileLoader file size: " + file.length + "  " + file.length.toInt)
//			    val buff = new Array[Byte](file.length.toInt)
			    val in = new FileInputStream(file)
//			    in.read(buff)
		    	stream = new DataInputStream(in) //new ByteArrayInputStream(buff))

//          val byt = scala.io.Source.fromFile(a).toArray
//          stream = new DataInputStream(new ByteArrayInputStream(byt.map(_.toByte)))
      }
    }
    def nextByte:Int = { count += 1;  stream.readByte & 0xff }
		def nextU1:Byte = { count += 1; stream.readByte }
		def nextU2:Int = { 
		  count += 2
		  try { 
		    stream.readChar.toInt 
		    } catch {
		      case e:Exception => println(">>ERR FileLoader-count 0x" + count.toHexString); throw e
		    }
		}
		def nextU4:Int = { count += 4; stream.readInt }
		def nextLong:Long = { count += 4; stream.readLong }
      

  }
} 
