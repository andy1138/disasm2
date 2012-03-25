package com.scalalabs.disasm.format

import com.scalalabs.disasm.classfile._
import scala.util.logging.Logged
import scala.collection.mutable.ListBuffer

trait JasminFormat extends FormatRepositoryComponent with Logged {

  def out(s: String) { println(s) }

  def formatter = new JasminFormatImpl

  class JasminFormatImpl extends FormatRepository {
    def format(clazzFile: ClazzFile): List[String] = {

      log("Start")

      val l = header(clazzFile) :::
        fields(clazzFile) :::
        methodNames(clazzFile).map(methodOut(_, clazzFile)).flatten :::
        Nil

      l.foreach(i => out(i))

      log("End")

      l
    }

    def header(clazzFile: ClazzFile): List[String] = {
      "" ::
        "" ::
        ".magic 0x" + clazzFile.magic.toHexString ::
        ".bytecode " + clazzFile.major_version + "." + clazzFile.minor_version ::
        ".source " + sourceFile(clazzFile).getOrElse("ERR-NoSource") ::
        (clazzSpec(clazzFile).fold(err => err, res => res)) ::
        superSpec(clazzFile) :::
        implements(clazzFile) :::
        signature(clazzFile) :::
        enclosingMethods(clazzFile) :::
        innerClazzes(clazzFile) ::: //.foreach(i => out(i))
        innerInterfaces(clazzFile) :::
        "" ::
        Nil

    }

    //  }

    def clazzSpec(clazzFile: ClazzFile): Either[String, String] = {
      import util.control.Exception.allCatch

      allCatch either {
        for {
          acc <- clazzAccess(clazzFile.access_flags)
          clazz <- clazzName(clazzFile, clazzFile.this_class)
        } return Right(".class " + acc + " " + clazz)
      } match {
        case Left(exception) => Left("")
        case Right(result) => Right(result.toString)
        case _ => Left("")
      }

      //    out(".class " + clazzAccess( clazzFile.access_flags) + " " + clazzName(clazzFile, clazzFile.this_class ).getOrElse("ERROR") )
    }

    def superSpec(clazzFile: ClazzFile): List[String] = {
      ".super " + clazzName(clazzFile, clazzFile.super_class).getOrElse("ERROR") :: Nil
    }

    def implements(clazzFile: ClazzFile): List[String] = {
      clazzFile.interfaces.map { i =>
        ".implements " + clazzName(clazzFile, i).getOrElse("ERROR")
      }.toList
    }

    def signature(clazzFile: ClazzFile): List[String] = {
      ".signature \" \"" :: Nil
    }

    def enclosingMethods(clazzFile: ClazzFile): List[String] = {
      findAttr[EnclosingMethod_attribute](clazzFile.attributes, ClazzFile.ATTRIB_EnclosingMethod).map(i => (".enclosing method " + clazzFile.constantPool.classInfo(i.class_index))).toList
    }

    def innerClazzes(clazzFile: ClazzFile): List[String] = {
      findAttr[InnerClasses_attribute](clazzFile.attributes, ClazzFile.ATTRIB_InnerClasses).map(_.clazzes map (j =>
        ".inner class  " + clazzFile.constantPool.classInfo(j.inner_class_info_index) + " " + clazzFile.constantPool.classInfo(j.outer_class_info_index) + " " + clazzFile.constantPool.utfString(j.inner_name_index) + " " + innerClazzAccess(j.inner_class_access_flags).toList.mkString(" "))).head.toList
    }

    def innerInterfaces(clazzFile: ClazzFile): List[String] = {
      ".inner interface " :: Nil
    }

    def fields(clazzFile: ClazzFile): List[String] = {
      clazzFile.fields.foldLeft(List.empty[String]) { (l, f) =>
        ".field " + fieldAccess(f.access_flags) + " " + clazzFile.constantPool.utfString(f.name_index) + " " + clazzFile.constantPool.utfString(f.descriptor_index) ::
          ".end field\n" :: l
      }
    }

    def methodNames(clazzFile: ClazzFile): List[String] = {
      clazzFile.methods.map { m =>
        methodName(m, clazzFile)
      }.toList
    }

    def methodOut(mthName: String, clazzFile: ClazzFile): List[String] = {
      var lst = List.empty[String]
      clazzFile.methods.filter(m => methodName(m, clazzFile) == mthName).foreach { m =>
        val mlst = (".method " + methodAccess(m.access_flags) + " " + clazzFile.constantPool.utfString(m.name_index) + "" + clazzFile.constantPool.utfString(m.descriptor_index)) ::
          findCodeAttr(m.attributes).map(outMethodCode(_, clazzFile)).getOrElse(List.empty[String]) :::
          ".end\n" :: Nil

        lst = lst ::: mlst
      }
      lst.toList
    }

    def methodName(m: method_info, clazzFile: ClazzFile) = clazzFile.constantPool.utfString(m.name_index) + clazzFile.constantPool.utfString(m.descriptor_index)

    //  def methods(clazzFile:ClazzFile) {
    //  	clazzFile.methods.foreach{ m =>
    //  	out(".method " + methodAccess(m.access_flags) + " " + clazzFile.constantPool.utfString(m.name_index) + "" + clazzFile.constantPool.utfString(m.descriptor_index) )
    //  	
    //  	findCodeAttr(m.attributes).foreach(outMethodCode(_, clazzFile))
    //  	
    //  	out(".end\n")
    //  	}
    //  }

    def outMethodCode(code: Code_attribute, clazzFile: ClazzFile): List[String] = {
      val lst = ListBuffer[String]()
      lst += ".limit stack " + code.max_stack
      lst += ".limit locals " + code.max_locals

      val x = new CodeAttrib(code.code, clazzFile.constantPool)
      val asm = x.disAsmAttrib(code.code, code.annotations)

      val srcLine = findLineAttr(code.annotations).map(_.lines.toList)

      asm.foreach(opLine => {

        srcLine.map(_.filter(_.start_pc == opLine.pc).foreach(lne => lst += (".line " + lne.line_number)))

        lst += ("  " + opLine)

      })

      code.exceptions.foreach(exp => (lst += ".exception " + exp))
      code.annotations.foreach(mth => { lst += ".annotation " + mth })
      lst.toList
    }

    def srcLineNumber(srcLine: Option[List[com.scalalabs.disasm.classfile.LNT_Lines]], opLine: AsmLine) {
      srcLine.map(_.filter(_.start_pc == opLine.pc).map(lne => ".line " + lne.line_number).foreach(i => out(i)))
    }

    def methodAccess(access: Int): String = {
      import ClazzFile._
      var acc = scala.collection.mutable.ListBuffer[String]()

      if ((access & JAVA_ACC_PUBLIC) != 0) acc += "public"
      if ((access & JAVA_ACC_PRIVATE) != 0) acc += "private"
      if ((access & JAVA_ACC_PROTECTED) != 0) acc += "protected"
      if ((access & JAVA_ACC_STATIC) != 0) acc += "static"
      if ((access & JAVA_ACC_FINAL) != 0) acc += "final"
      if ((access & JAVA_ACC_SYNCHRONIZED) != 0) acc += "synchronized"
      if ((access & JAVA_ACC_BRIDGE) != 0) acc += "bridge"
      if ((access & JAVA_ACC_VARARGS) != 0) acc += "varargs"
      if ((access & JAVA_ACC_NATIVE) != 0) acc += "native"
      if ((access & JAVA_ACC_ABSTRACT) != 0) acc += "abstract"
      if ((access & JAVA_ACC_STRICT) != 0) acc += "strict"
      if ((access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"

      acc.mkString(" ")
      //   access.toHexString + " " + acc.mkString(" ") 
    }

    def clazzName(clazzFile: ClazzFile, idx: Int): Option[String] = {
      clazzFile.constantPool.get(idx) match {
        case CONSTANT_Class_info(cidx) => Some(clazzFile.constantPool.utfString(cidx))
        case _ => None //"ERR-clazzName: " + idx + "  " + clazzFile.constantPool.at(idx)
      }
    }

    def clazzAccess(access: Int): Option[String] = {
      import ClazzFile._
      var acc = scala.collection.mutable.ListBuffer[String]()

      if ((access & JAVA_ACC_PUBLIC) != 0) acc += "public"
      if ((access & JAVA_ACC_FINAL) != 0) acc += "final"
      if ((access & JAVA_ACC_SUPER) != 0) acc += "super"
      if ((access & JAVA_ACC_INTERFACE) != 0) acc += "interface"
      if ((access & JAVA_ACC_ABSTRACT) != 0) acc += "abstract"
      if ((access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
      if ((access & JAVA_ACC_ANNOTATION) != 0) acc += "annotation"
      if ((access & JAVA_ACC_ENUM) != 0) acc += "enum"

      Some(acc.mkString(" "))
    }

    def innerClazzAccess(access: Int): Option[String] = {
      import ClazzFile._
      var acc = scala.collection.mutable.ListBuffer[String]()

      if ((access & JAVA_ACC_PUBLIC) != 0) acc += "public"
      if ((access & JAVA_ACC_PRIVATE) != 0) acc += "private"
      if ((access & JAVA_ACC_PROTECTED) != 0) acc += "protected"
      if ((access & JAVA_ACC_STATIC) != 0) acc += "static"
      if ((access & JAVA_ACC_FINAL) != 0) acc += "final"
      if ((access & JAVA_ACC_INTERFACE) != 0) acc += "interface"
      if ((access & JAVA_ACC_ABSTRACT) != 0) acc += "abstract"
      if ((access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
      if ((access & JAVA_ACC_ANNOTATION) != 0) acc += "annotation"
      if ((access & JAVA_ACC_ENUM) != 0) acc += "enum"

      Some(acc.mkString(" "))
    }

    def fieldAccess(access: Int): String = {
      import ClazzFile._
      var acc = scala.collection.mutable.ListBuffer[String]()

      if ((access & JAVA_ACC_PUBLIC) != 0) acc += "public"
      if ((access & JAVA_ACC_PRIVATE) != 0) acc += "private"
      if ((access & JAVA_ACC_PROTECTED) != 0) acc += "protected"
      if ((access & JAVA_ACC_STATIC) != 0) acc += "static"
      if ((access & JAVA_ACC_FINAL) != 0) acc += "final"
      if ((access & JAVA_ACC_VOLATILE) != 0) acc += "volatile"
      if ((access & JAVA_ACC_TRANSIENT) != 0) acc += "transient"
      if ((access & JAVA_ACC_SYNTHETIC) != 0) acc += "synthetic"
      if ((access & JAVA_ACC_ENUM) != 0) acc += "enum"

      acc.mkString(" ")
    }

    def sourceFile(clazzFile: ClazzFile): Option[String] = {
      findSourceAttr(clazzFile.attributes).map(s => clazzFile.constantPool.utfString(s.sourcefile_index))
    }

    def findSourceAttr(a: Array[attribute_info]): Option[SourceFile_attribute] = findAttr(a, ClazzFile.ATTRIB_SourceFile)

    def findCodeAttr(a: Array[attribute_info]): Option[Code_attribute] = findAttr(a, ClazzFile.ATTRIB_Code)

    def findLineAttr(a: Array[attribute_info]): Option[LineNumberTable_attribute] = findAttr(a, ClazzFile.ATTRIB_LineNumberTable)

    def findAttr[T <: attribute_info](a: Array[attribute_info], s: String): Option[T] = {
      if (a.isEmpty) return None

      val x: Array[attribute_info] = a.filter { f: attribute_info => if (f.name == s) true else false }

      if (x.isEmpty) None else Some(x(0).asInstanceOf[T])
    }

  }

} 
