package com.scalalabs.disasm.input

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class FileLoaderSuite extends FunSuite with ShouldMatchers {

  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 49
  final val JAVA_MINOR_VERSION = 0

  class TestObj
  
  
  test("size test 1bit") {
  	val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello$.class"
  			
		val t = new TestObj with FileLoader 
  			
		t.loader.load(filename)
		
		var count =0
		try {
		  while( true ) {
		  	val x = t.loader.nextU1
//		  	print( x.toInt.toHexString + " ")
		    count += 1
		  }
		} catch {
		  case e:Exception => println("1bit loop count: " + count)
		}
  }
  
  test("size test 2bit") {
  	val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello$.class"
  			
  			val t = new TestObj with FileLoader 
  			
  			t.loader.load(filename)
  			
  			var count =0
  			try {
  				while( true ) {
  					val x = t.loader.nextU2
  							count += 2
		  	print( x.toInt.toHexString + " ")
  				}
  			} catch {
  			case e:Exception => println("2bit loop count: " + count)
  			}
  }
  
  test("size test 4bit") {
  	val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello$.class"
  			
  			val t = new TestObj with FileLoader 
  			
  			t.loader.load(filename)
  			
  			var count =0
  			try {
  				while( true ) {
  					val x = t.loader.nextU4
  							count += 4
  				}
  			} catch {
  			case e:Exception => println("4bit loop count: " + count)
  			}
  }

  
  
  
  test("basic") {

    val filename = "./target/scala-2.9.1/sbt-0.11.2/test-classes/com/scalalabs/disasm/input/Hello.class"
      
    val t = new TestObj with FileLoader
    t.loader.load(filename)
    println( "FileLoaderSuite-basic " + t.loader.stream )
    
    val magic = t.loader.nextU4
    assert( magic === JAVA_MAGIC, "Incorrect Magic 0x" + magic.toHexString)
    
    val minor = t.loader.nextU2
    assert( minor === JAVA_MINOR_VERSION, "Inocrrect Minor")

    val major = t.loader.nextU2
    assert( major === JAVA_MAJOR_VERSION, "Incorrect Major: " +major)
    
    
  }

}