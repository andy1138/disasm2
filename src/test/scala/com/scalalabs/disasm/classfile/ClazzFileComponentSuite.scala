package com.scalalabs.disasm.classfile

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import com.scalalabs.disasm.input.FileLoader
import com.scalalabs.disasm._
import com.scalalabs.disasm.format.JasminFormat

@RunWith(classOf[JUnitRunner])
class ClazzFileComponentSuite extends FunSuite with ShouldMatchers {

  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 49
  final val JAVA_MINOR_VERSION = 0

  class TestObj
  
  ignore("basic") {

    val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello.class"
      
          val t = new ClazzFileComponent with FileLoader with JasminFormat with StdOut

//    val t = new TestObj with com.scalalabs.disasm.classfile.ClazzFileComponent with FileLoader
    t.loader.load(filename)
    println( "S " + t.loader.stream )
    val clazzFile = t.parse
    import clazzFile._
    
//    val magic = t.loader.nextU4
    assert( magic === JAVA_MAGIC, "Incorrect Magic 0x" + magic.toHexString)
//    
//    val minor = t.loader.nextU2
    assert( minor_version === JAVA_MINOR_VERSION, "Inocrrect Minor")
//
//    val major = t.loader.nextU2
    assert( major_version === JAVA_MAJOR_VERSION, "Incorrect Major: " +major_version)
    
    t.formatter.format(clazzFile)
  }

  test("basic-2") {
  	
  	val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello.class"
  			
		val t = new ClazzFileComponent with FileLoader with JasminFormat with StdOut
  			
		t.loader.load(filename)
		println( "S " + t.loader.stream )
		val clazzFile = t.parse
		import clazzFile._
		
  	t.formatter.format(clazzFile)
  }
  
}