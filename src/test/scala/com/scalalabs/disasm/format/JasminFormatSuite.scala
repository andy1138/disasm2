package com.scalalabs.disasm.format

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import com.scalalabs.disasm.classfile.IC_classes
import com.scalalabs.disasm.classfile.InnerClasses_attribute
import com.scalalabs.disasm.classfile.ClazzFile
import com.scalalabs.disasm.classfile.CPool
import com.scalalabs.disasm.classfile.cp_info
import com.scalalabs.disasm.classfile.attribute_info
import com.scalalabs.disasm.classfile.CONSTANT_Dummy
import com.scalalabs.disasm.classfile.CONSTANT_Class_info
import com.scalalabs.disasm.classfile.CONSTANT_Utf8_info
import com.scalalabs.disasm.classfile.method_info

@RunWith(classOf[JUnitRunner])
class JasminFormatSuite extends FunSuite with ShouldMatchers {

  test("method names") {
    
    val methodList = 
      method_info(0, 1, 2, Nil.toArray) ::
    	method_info(0, 3, 4, Nil.toArray) ::
      Nil
    
    val cpList:List[cp_info] = 
      CONSTANT_Dummy() ::
      CONSTANT_Utf8_info("<init>".getBytes) ::
    	CONSTANT_Utf8_info("()V".getBytes) ::
    	CONSTANT_Utf8_info("main".getBytes) ::
  		CONSTANT_Utf8_info("()V".getBytes) ::
      Nil 
    
    val cpool = new CPool
    cpool.add( cpList.toArray )
    
    val clazzfile = new ClazzFile(0xcafababe, 0, 0, cpool, 0, 0, 0, Nil.toArray, Nil.toArray, methodList.toArray, Nil.toArray )
    clazzfile.constantPool = cpool
    
    val j = new Object with JasminFormat
    val res = j.formatter.methodNames(clazzfile)
    
    assert( res === "<init>()V" :: "main()V" :: Nil)
    
    
  }
  
  test("Inner Clazzes") {

    val icc = new scala.collection.mutable.ArrayBuffer[IC_classes]
    icc += IC_classes(1,1,3,0) 
    val attributes = InnerClasses_attribute( icc.toArray) :: List.empty[attribute_info]

    
    val cpList:List[cp_info] = 
      CONSTANT_Dummy() ::
      CONSTANT_Class_info(2) ::
      CONSTANT_Utf8_info("Hello".getBytes) ::
    	CONSTANT_Utf8_info("World".getBytes) ::
      Nil
    
    val cpool = new CPool
    cpool.add( cpList.toArray )
    
    val clazzfile = new ClazzFile(0xcafababe, 0, 0, cpool, 0, 0, 0, Nil.toArray, Nil.toArray, Nil.toArray, attributes.toArray )
    clazzfile.constantPool = cpool
    
    val j = new Object with JasminFormat
    val res = j.formatter.innerClazzes(clazzfile)
    
    assert( res.head === ".inner class  Hello Hello World ")
    
  }

}