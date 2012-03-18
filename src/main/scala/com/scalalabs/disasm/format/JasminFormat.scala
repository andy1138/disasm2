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
	    println(".source " + sourceFile(clazzFile))
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
    println(".class " + clazzAccess( clazzFile.access_flags) + " " + clazzName(clazzFile, clazzFile.this_class ) )
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
    	println(".field " + fieldAccess(f.access_flags) + " " + clazzFile.constantPool.utfString(f.name_index) + " " + clazzFile.constantPool.utfString(f.descriptor_index) )

    	
    	println(".end field\n")
    }
  }
  
  def methods(clazzFile:ClazzFile) {
    clazzFile.methods.foreach{ m =>
    	println(".method " + methodAccess(m.access_flags) + " " + clazzFile.constantPool.utfString(m.name_index) + " " + clazzFile.constantPool.utfString(m.descriptor_index) )

//    	m.attributes.foreach( i => println(">> methods attr " + i))
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

  def methodAccess(access:Int): String = {
    import ClazzFile._
    var acc = scala.collection.mutable.ListBuffer[String]()
      
    if( (access & JAVA_ACC_PUBLIC) != 0) acc += "public"
	  if( (access & JAVA_ACC_PRIVATE) != 0) acc += "private"
	  if( (access & JAVA_ACC_PROTECTED) != 0) acc += "protected"
	  if( (access & JAVA_ACC_STATIC) != 0) acc += "static"
	  if( (access & JAVA_ACC_FINAL)  != 0) acc += "final"
	  if( (access & JAVA_ACC_SYNCHRONIZED) != 0) acc += "synchronized"
	  if( (access & JAVA_ACC_BRIDGE) != 0) acc += "bridge"
	  if( (access & JAVA_ACC_VARARGS) != 0) acc += "varargs"
	  if( (access & JAVA_ACC_NATIVE) != 0) acc += "native"
	  if( (access & JAVA_ACC_ABSTRACT) != 0) acc += "abstract"
	  if( (access & JAVA_ACC_STRICT) != 0) acc += "strict"
	  if( (access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
	    
   acc.mkString(" ") 
//   access.toHexString + " " + acc.mkString(" ") 
  }
  
  def clazzName(clazzFile:ClazzFile, idx:Int):String = {
    clazzFile.constantPool.get(idx) match {
      case CONSTANT_Class_info(cidx) => clazzFile.constantPool.utfString(cidx)
      case _ => "ERR-clazzName: " + idx + "  " + clazzFile.constantPool.at(idx)
    }
  }
  
  def clazzAccess( access:Int): String = {
    import ClazzFile._
    var acc = scala.collection.mutable.ListBuffer[String]()
   
		if( (access & JAVA_ACC_PUBLIC) != 0) acc += "public"
		if( (access & JAVA_ACC_FINAL) != 0) acc += "final"
		if( (access & JAVA_ACC_SUPER) != 0) acc += "super"
		if( (access & JAVA_ACC_INTERFACE) != 0) acc += "interface"
		if( (access & JAVA_ACC_ABSTRACT) != 0) acc += "abstract"
		if( (access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
		if( (access & JAVA_ACC_ANNOTATION) != 0) acc += "annotation"
		if( (access & JAVA_ACC_ENUM) != 0) acc += "enum"
   
    acc.mkString(" ") 
  }

  def fieldAccess( access:Int): String = {
		import ClazzFile._
		var acc = scala.collection.mutable.ListBuffer[String]()
		
		if( (access & JAVA_ACC_PUBLIC) != 0) acc += "public"
		if( (access & JAVA_ACC_PRIVATE) != 0) acc += "private"
		if( (access & JAVA_ACC_PROTECTED) != 0) acc += "protected"
		if( (access & JAVA_ACC_STATIC) != 0) acc += "static"
		if( (access & JAVA_ACC_FINAL) != 0) acc += "final"
		if( (access & JAVA_ACC_VOLATILE) != 0) acc += "volatile"
		if( (access & JAVA_ACC_TRANSIENT) != 0) acc += "transient"
		if( (access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
		if( (access & JAVA_ACC_ENUM) != 0) acc += "enum"
		
		acc.mkString(" ") 
  }
  
  def sourceFile(clazzFile:ClazzFile):String = {
    findSourceAttr( clazzFile.attributes) match {
      case Some(s) => clazzFile.constantPool.utfString(s.sourcefile_index)
      case None => "ERR-sourceFile: " 
    }
  }
  
  def methodName(clazzFile:ClazzFile, idx:Int):String = {
  	clazzFile.constantPool.get(idx) match {
  	case CONSTANT_Class_info(cidx) => clazzFile.constantPool.utfString(cidx)
  	case _ => "ERR-methodName"
  	  
  	}
  }
  
  def findSourceAttr(a:Array[attribute_info]):Option[SourceFile_attribute] = findAttr(a, ClazzFile.ATTRIB_SourceFile)

  def findCodeAttr(a:Array[attribute_info]):Option[Code_attribute] = findAttr(a, ClazzFile.ATTRIB_Code)
  
  def findLineAttr(a:Array[attribute_info]):Option[LineNumberTable_attribute] = findAttr(a, ClazzFile.ATTRIB_LineNumberTable)
  
  def findAttr[T <: attribute_info](a:Array[attribute_info], s:String):Option[T] = {
    if( a.isEmpty) return None
 
    val x:Array[attribute_info] = a.filter { f:attribute_info => if( f.name == s) true else false }
    
  	if( x.isEmpty) None else Some( x(0).asInstanceOf[T] )
  }
  
  
  
} 
