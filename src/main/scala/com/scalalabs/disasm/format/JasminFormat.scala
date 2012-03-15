package com.scalalabs.disasm.format

//import com.scalalabs.disasm.classfile.ClazzFile
import com.scalalabs.disasm.classfile._


trait JasminFormat extends FormatRepositoryComponent {
  
  def formatter = new JasminFormatImpl
  
  class JasminFormatImpl extends FormatRepository {
    def format(clazzFile:ClazzFile):List[String] =  {
	    
	    println
	    println
	    println(".magic 0x" + clazzFile.magic.toHexString)
	    println(".bytecode " + clazzFile.major_version+"."+ clazzFile.minor_version)
	    println(".source "  )
	    clazzSpec(clazzFile)
	    superSpec(clazzFile)
	    implements(clazzFile)
	    signature(clazzFile)
	    embededMethods(clazzFile)
	    
	    fields(clazzFile)
	    methods(clazzFile)
	    
	     "*NullFormatImpl*" :: Nil
    }
    
  }
  
  
  def clazzSpec(clazzFile:ClazzFile) {
    println(".class 0x" + clazzFile.access_flags.toHexString + " " + clazzName(clazzFile, clazzFile.this_class ) )
  }
  
  def superSpec(clazzFile:ClazzFile) {
  	println(".super " + clazzName(clazzFile, clazzFile.super_class ) )
  }
  

  def implements(clazzFile:ClazzFile) {
    clazzFile.interfaces.foreach{ i =>
    	println(".implements " + clazzName(clazzFile, i ) )
    }
  }
  
  def signature(clazzFile:ClazzFile) {
  	println(".signature \" \""  )
  }

  def embededMethods(clazzFile:ClazzFile) {
  	println(".enclosing method "  )
  }

  def fields(clazzFile:ClazzFile) {
    clazzFile.fields.foreach{ f =>
    	println(".field 0x" + f.access_flags.toHexString + " " + clazzFile.constantPool.utfString(f.name_index) + " " + clazzFile.constantPool.utfString(f.descriptor_index) )

    	
    	println(".end field\n")
    }
  }
  
  def methods(clazzFile:ClazzFile) {
    clazzFile.methods.foreach{ m =>
    	println(".method 0x" + m.access_flags.toHexString + " " + clazzFile.constantPool.utfString(m.name_index) + " " + clazzFile.constantPool.utfString(m.descriptor_index) )

    	m.attributes.foreach( i => println(">> methods attr " + i))
    	findCodeAttr(m.attributes) match {
	  	  case Some(c) =>
	  	    println( ".limit stack " + c.max_stack)
	  	    println( ".limit locals " + c.max_locals)
        	val x = new CodeAttrib(c.code, clazzFile.constantPool)
	  	    val l = x.disAsmAttrib(c.code, c.annotations)

	  	    val srcLine:Option[List[LNT_Lines]] = findLineAttr(c.annotations) match {
		    	  case Some(s) =>  Some(s.lines.toList)
		    	  case None => None
		    	}

	  	    
	  	    l.foreach( opLine => {
	  	      
	  	      srcLine match {
	  	        case Some(s) => s.filter( _.start_pc == opLine.pc).foreach( lne => println(".line " + lne.line_number))
	  	        case None => 
	  	      }
	  	      println("  " + opLine)
	  	      })

	  	      c.exceptions.foreach( exp => println(".exception " + exp))
	  	      c.annotations.foreach( mth => { println(".annotation " + mth)})
	  	      
	  	  case None =>
    	}  

    	println(".end\n")
    }
  }

  
  def clazzName(clazzFile:ClazzFile, idx:Int) = {
    clazzFile.constantPool.get(idx) match {
      case CONSTANT_Class_info(cidx) => clazzFile.constantPool.utfString(cidx)
      case _ => "ERR1"
    }
  }
  
  def methodName(clazzFile:ClazzFile, idx:Int) = {
  	clazzFile.constantPool.get(idx) match {
  	case CONSTANT_Class_info(cidx) => clazzFile.constantPool.utfString(cidx)
  	case _ => "ERR1"
  	  
  	}
  }
  

  def findCodeAttr(a:Array[attribute_info]):Option[Code_attribute] = findAttr(a, ClazzFile.ATTRIB_Code)
  
  def findLineAttr(a:Array[attribute_info]):Option[LineNumberTable_attribute] = findAttr(a, ClazzFile.ATTRIB_LineNumberTable)
  
  def findAttr[T <: attribute_info](a:Array[attribute_info], s:String):Option[T] = {
    if( a.isEmpty) return None
 
    val x:Array[attribute_info] = a.filter { f:attribute_info => if( f.name == s) true else false }
    
  	if( x.isEmpty) None else Some( x(0).asInstanceOf[T] )
  }
  
  
  
} 
