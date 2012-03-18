package com.scalalabs.disasm.classfile


import com.scalalabs.disasm.OutputRepositoryComponent
import com.scalalabs.disasm.input.LoaderRepositoryComponent

trait ClazzFileComponent extends ClazzFileRepositoryComponent {
  this: LoaderRepositoryComponent with OutputRepositoryComponent =>
    
  def clazzFile = new ClazzFileImpl
  
  def parse = clazzFile.parse
  
  class ClazzFileImpl extends ClassFileRepository {
    

    def parse():ClazzFile =  {
      val loader1 = loader
      import loader1._
    	import ClazzFile._
    
    	
    	
    	def loop[T](l:Int, f: => T) = {
        List.tabulate( l ) { a =>  f}
      }
    	
      
      val cpool = new CPool
      
    	def parseConstantPool(): cp_info = {
    	  val tag = nextU1
    	  val t = tag match {
    	    case CONSTANT_UTF8 => CONSTANT_Utf8_info( loop( nextU2, nextU1 ).toArray ) 
//    	    case ClassFile.CONSTANT_UNICODE
    	    case CONSTANT_INTEGER => CONSTANT_Integer_info( nextU4 )
    	    case CONSTANT_FLOAT => CONSTANT_Float_info( nextU4 )
    	    case CONSTANT_LONG => CONSTANT_Long_info( nextU4, nextU4  )
    	    case CONSTANT_DOUBLE  => CONSTANT_Double_info( nextU4, nextU4  )
    	    case CONSTANT_CLASS  => CONSTANT_Class_info( nextU2 )
    	    case CONSTANT_STRING => CONSTANT_String_info( nextU2 )
    	    case CONSTANT_FIELDREF => CONSTANT_Fieldref_info( nextU2, nextU2 )
    	    case CONSTANT_METHODREF => CONSTANT_Methodref_info( nextU2, nextU2 )
    	    case CONSTANT_INTFMETHODREF => CONSTANT_InterfaceMethodref_info( nextU2, nextU2 )
    	    case CONSTANT_NAMEANDTYPE => CONSTANT_NameAndType_info( nextU2, nextU2 )
    	    case CONSTANT_MethodHandle => CONSTANT_MethodHandle_info( nextU2, loader.nextU2 )
    	    case CONSTANT_MethodType => CONSTANT_MethodType_info( nextU2 )
    	    case CONSTANT_InvokeDynamic => CONSTANT_InvokeDynamic_info( nextU2, nextU2 )
    	    case _ => println( ">> ERR parseConstantPool " + tag); throw new Exception(">> ERR parseConstantPool " + tag)
    	  }
//    	  println(">>parseConstantPool tag: " + tag + " res: " + t)
    	  
    	  t
    	}
    	
      def cpLoop[T](l:Int, f: => T):List[cp_info] = {
//        List.tabulate( l ) { a =>  parseConstantPool()}
        val lst = scala.collection.mutable.ListBuffer[cp_info]()
        var i =0
        while( i < l) {
          val item = parseConstantPool()
          lst += item
          item match {
            case x:CONSTANT_Long_info => i += 1
            case x:CONSTANT_Double_info => i += 1
            case _ =>
          }
          i += 1
        }
        lst.toList
      }

      
    	def fieldInfo():field_info = {
    	  new field_info( nextU2, nextU2, nextU2, loop(nextU2, attributeInfo()).toArray )
    	}
    	
    	
    	def methodInfo():method_info = {
    	  val access_flags = nextU2
    	  val name_index = nextU2
    	  val descriptor_index = nextU2
//    	  println(">>methodInfo: 0x" +  access_flags.toHexString + " " + cpool.utfString(name_index) + " " + cpool.utfString(descriptor_index))
    	  new method_info( access_flags, name_index, descriptor_index, loop(nextU2, attributeInfo()).toArray )
    	}
    	
    	def attributeInfo():attribute_info = {
    	  val nameIdx = nextU2
    	  val attrLength = nextU4
    	  
    	  val name = cpool.get(nameIdx) match {
    	    case CONSTANT_Utf8_info( s ) => new String(s)
    	    case _ => "*Unknown*"
    	  }
    	  
//    	  println(">>loading " +nameIdx.toHexString + " " + name + " len: " + attrLength)
    	  
    	  val att = name match {
    	    case ATTRIB_ConstantValue => ConstantValue_attribute( nextU2 )
    	    case ATTRIB_SourceFile => SourceFile_attribute( nextU2 )
//    	    case ATTRIB_InnerClasses => 
//    	      val loopCnt = nextU2
//    	      println("InnerClasses_attribute loop: " + loopCnt  )
//    	      InnerClasses_attribute( loop(loopCnt, IC_classes(nextU2, nextU2, nextU2, nextU2)).toArray )
    	    case ATTRIB_RuntimeVisibleAnnotations => RuntimeVisibleAnnotations_attribute( 
    	        			loop(nextU2, loadRVAAnnotations() ).toArray )
    	    case ATTRIB_Signature => Signature_attribute( nextU2 )
    	    case ATTRIB_Code => Code_attribute( 
    	        nextU2, 
    	        nextU2, 
    	        loop(nextU4, nextU1 ).toArray, 
    	        loop(nextU2, loadExceptions() ).toArray, 
    	        loop(nextU2, attributeInfo() ).toArray 
    	        )
    	    case ATTRIB_LineNumberTable => LineNumberTable_attribute( loop( nextU2, LNT_Lines( nextU2, nextU2)).toArray )
//    	    case ClazzFile.ATTRIB_ScalaSig => ScalaSig_attribute( List.tabulate(loader.nextU2) { _ => InnerClasses_attribute_classes(loader.nextU2, loader.nextU2, loader.nextU2, loader.nextU2)}.toArray )
    	    case _ => new UnknownAttribute(name,  loop(attrLength, nextU1).toArray )
    	  }
//    	  att match {
//    	    case SourceFile_attribute(n) => 
//    	      println("SourceFile: " + cpool.utfString(n))
//    	    case Signature_attribute(s) =>
//    	      println("Signature: " + cpool.utfString(s))
//    	    case InnerClasses_attribute(l) => 
//    	      l.foreach { i =>
//    	        println( "InnerClasses Inner: " + cpool.classInfo(i.inner_class_info_index) + " Outer: " + cpool.classInfo(i.outer_class_info_index) + " IName: " + cpool.utfString(i.inner_name_index) +" Flags: 0x" + i.inner_class_access_flags.toHexString )
//    	      }  
//    	    case a => println("Attr Other: " + a)
//    	  }
    	  att
    	}
    	
    	
//  RuntimeVisibleAnnotations_attribute

			def loadRVAAnnotations() = {
    	  val type_index = nextU2
//    	  println("loadRVAAnnotations type: " + cpool.utfString(type_index))
	    	RVA_annotation(type_index, loop(nextU2, loadRVAElementValuePair() ).toArray )  
	    	}    	
				
			def loadRVAElementValuePair() = {
			  val element_name_index = nextU2
//			  println("loadRVAElementValuePair '" + cpool.utfString(element_name_index) + "'")
			  RVA_element_value_pairs(element_name_index, loop(1, loadRVAElementValue() ).toArray ) // TODO 1
			}    	
			
			def loadRVAElementValue():RVA_element_value = {
			  val tag = nextU1 
			  tag match {
			    case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' => RVA_const_value_index(tag, nextU2)
			    case 'e' => RVA_enum_const_value( tag, loader.nextU2, nextU2 )
			    case 'c' => RVA_class_info_index( tag, loader.nextU2 )
			    case '[' => RVA_array_value( tag, loop(loader.nextU2, loadRVAElementValue() ).toArray )
			//    case 's' => 
			//    case '@' => RVA_annotation_value( tag, loadRVAAnnotations())
			    case _ => println("ERR: loadRVAElementValue: tag:" + tag); RVA_const_value_index(tag, 0)
			  }
			}
			    	
			
			def loadExceptions():exception_table = {
			  exception_table(loader.nextU2, loader.nextU2, loader.nextU2, loader.nextU2)
			}
    	
    	      val dummy:cp_info = CONSTANT_Dummy() // as cpool is 1 based add dummy zero item
      
    	
    	val magic = loader.nextU4
    	val minor_version = loader.nextU2
    	val major_version = loader.nextU2
    	   
    	val poolCount = loader.nextU2 -1
    	cpool.add( ((dummy :: cpLoop(poolCount, parseConstantPool ))).toArray )
    	val constantPool = cpool 

//    	cpool.dump
    	
//    	val constantPool = ((dummy :: List.tabulate(loader.nextU2 - 1) { _ => parseConstantPool })).toArray
    	val access_flags = loader.nextU2
    	val this_class = loader.nextU2
    	val super_class = loader.nextU2
// println(">>Loading interfaces")   	
    	val interfaces = loop(loader.nextU2, loader.nextU2 ).toArray
// println(">>Loading fields")   	
    	val fields = loop(loader.nextU2, fieldInfo() ).toArray
// println(">>Loading methods")   	
    	val methods = loop(loader.nextU2, methodInfo() ).toArray
    	
    	val attrCount = loader.nextU2
// println(">>Loading attributes cnt: " + attrCount)   	
    	val attributes = loop(attrCount,  attributeInfo() ).toArray
// println(">>Loading end")   	
    	
    	val clazz = new ClazzFile(
    	    magic, minor_version, major_version, constantPool, access_flags, this_class, super_class,
    	    interfaces, fields, methods, attributes 
    	    )

    	
//    	parseClass
//    	import clazz._
//    	magic = loader.nextByte
      clazz
    }
    
    
    def createResult() {}
    outputRepository.out("Hello")
  }
  
}
