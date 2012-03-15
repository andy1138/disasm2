package com.scalalabs.disasm
import java.io.DataInputStream
import java.io.ByteArrayInputStream
import java.security.InvalidParameterException
import com.scalalabs.disasm.input.LoaderRepositoryComponent
import com.scalalabs.disasm.input.FileLoader
import com.scalalabs.disasm.input.BytesLoader
import com.scalalabs.disasm.classfile.ClazzFileComponent
import com.scalalabs.disasm.format.NullFormat
import com.scalalabs.disasm.format.FormatRepositoryComponent


//trait LoaderRepositoryComponent {
//  type T
//  def loader: LoaderRepository[T]
//  
//  trait LoaderRepository[T] {
//    def load(x:T) 
//    
//    var stream: DataInputStream = _
//
//    def nextByte:Byte = stream.readByte
//		def nextChar:Char = stream.readChar
//		def nextInt:Int = stream.readInt
//		def nextLong:Long = stream.readLong
//
//  }
//}

///** Load from file
// */
//trait FileLoader extends LoaderRepositoryComponent {
//  type T = String
//  def loader = new FileLoaderImpl[T]
//  
//  class FileLoaderImpl[T] extends LoaderRepository[T] {
//    def load(s:T) {
//      s match {
//        case a:String => 
//          val byt = scala.io.Source.fromFile(a).toArray
//          stream = new DataInputStream(new ByteArrayInputStream(byt.map(_.toByte)))
//      }
//      
//    }
//  }
//} 

///** Load from byte array
// */
//trait BytesLoader extends LoaderRepositoryComponent {
//  type T  = Array[Byte]
//  def loader = new BytesLoaderImpl[T]
//  
//  class BytesLoaderImpl[T] extends LoaderRepository[T] {
//    def load(b:T) {
//      b match {
//        case a:Array[Byte] => stream = new DataInputStream(new ByteArrayInputStream(a))
//        case _ => new InvalidParameterException("" + b)
//      }
//    }
//  }
//} 


//-------------

trait OutputRepositoryComponent {
  def outputRepository: OutputRepository
  
  trait OutputRepository {
    def out(s:String):Unit
  }
}

/** Output to stdout
 */
trait StdOut extends OutputRepositoryComponent {
  this: FormatRepositoryComponent =>
    
  def outputRepository = new StdOutImpl
  
  class StdOutImpl extends OutputRepository {
    def out(s:String):Unit = {
//    		val formatted = formatter.format(s)
//    		println(formatted)
    }
  }
} 

/** Output to stdout
 */
trait ListOut extends OutputRepositoryComponent {
  this: FormatRepositoryComponent =>
    
  def outputRepository = new StdOutImpl
  
  class StdOutImpl extends OutputRepository {
    def out(s:String):Unit = {
//    		val formatted = formatter.format(s)
//    		formatted :: Nil
    }
  }
} 


// -----------

//trait FormatRepositoryComponent {
//  def formatter: FormatRepository
//  
//  trait FormatRepository {
//    def format(s:String):String
//  }
//}

/** Do nothing formatter
 */
//trait NullFormat extends FormatRepositoryComponent {
//  def formatter = new NullFormatImpl
//  
//  class NullFormatImpl extends FormatRepository {
//    def format(s:String) = s
//  }
//} 




// ----

//class ClazzFile {
//  var magic:Int = 0
//}

//trait ClazzFileRepositoryComponent {
//  def clazzFile: ClassFileRepository
//  
//  trait ClassFileRepository {
//    def parse(): Unit
//    def createResult(): Unit
//  }
//}


//trait ClazzFileComponent extends ClazzFileRepositoryComponent {
//  this: LoaderRepositoryComponent with OutputRepositoryComponent =>
//    
//  def clazzFile = new ClazzFileImpl
//  
//  def parse = clazzFile.parse
//  
//  class ClazzFileImpl extends ClassFileRepository {
//    def parse()  {
//    	val clazz = new ClazzFile()
////    	import clazz._
////    	magic = loader.nextByte
//    }
//    def createResult() {}
//    outputRepository.out("Hello")
//  }
//}



object Test {
  def main( args:Array[String] ) {
    val by:Array[Byte] = Array[Byte](00, 00, 0xff.toByte)

    
    val a = new ClazzFileComponent with FileLoader with NullFormat with StdOut
    a.loader.load("filename.class")
    a.clazzFile.parse
    a.clazzFile.createResult
    
		val b = new ClazzFileComponent with BytesLoader with NullFormat with StdOut
    b.loader.load(by)
    b.clazzFile.parse
    b.parse
    
    
    class Test
    val c = new Test with BytesLoader
  }
}