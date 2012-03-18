package com.scalalabs.disasm.classfile

class ClazzFile (
  var magic:Int,
  var minor_version:Int,
  var major_version:Int,
  var constantPool:CPool,
  var access_flags:Int,
  var this_class:Int,
  var super_class:Int,
  var interfaces:Array[Int],
  var fields:Array[field_info],
  var methods:Array[method_info],
  var attributes:Array[attribute_info]
)

class CPool {
  private val items = new scala.collection.mutable.ArrayBuffer[cp_info]
  
  def add( a:Array[cp_info]) { a.foreach( add(_)) }
  def add( i:cp_info ) { 
    items += i 
    i match {
      case e:CONSTANT_Long_info  => items += CONSTANT_Dummy()
      case e:CONSTANT_Double_info  => items += CONSTANT_Dummy()
      case _ =>
    	}
    }
  def get( idx:Int) = items(idx)
  def at( idx:Int) = items(idx)
  
  
  def classInfo(idx:Int) = {
    items(idx) match {
      case CONSTANT_Class_info(cidx) => utfString(cidx)
      case _ => "ERR_C"
    }
  }
  
  def nameTypeInfo( idx:Int): String = {
    items(idx) match {
      case CONSTANT_NameAndType_info(n, t) => utfString(n) + ":" + utfString(t)
      case _ => "ERR-nameTypeInfo " + idx
    }
  }
  
  def utfString( idx:Int ) = {
    if( idx == 0 ) "" else
	    items(idx) match {
	      case CONSTANT_Utf8_info( bytes ) => new String( bytes )
	      case _ => "ERR-utfString " + idx
	    }
  }
  
  
  private def itemToString( i: cp_info ):String = {
    i match {
      case CONSTANT_Utf8_info(s) => new String(printStr(s))
      case _ => ""
    }
  }
  
  private def printStr( b:Array[Byte]): Array[Byte] = {
//    b.map( c => if( c.toChar.isLetterOrDigit || c.toChar.isWhitespace ) c else '.'.toByte )
    b
  }
  
  def dump() {
    var c = 0
    println(">> CPOOL size: " + items.length )
    items.foreach( i =>  { c += 1; println("[" + c + "] "  + i + " '" + itemToString(i) + "'" ) })
  }
} 

sealed class cp_info(val tag:Int) 

case class CONSTANT_Dummy(  ) extends cp_info(ClazzFile.CONSTANT_DUMMY)
case class CONSTANT_Class_info( name_index:Int ) extends cp_info(ClazzFile.CONSTANT_CLASS)
case class CONSTANT_Fieldref_info( class_index:Int, name_and_type_index:Int) extends cp_info(ClazzFile.CONSTANT_FIELDREF)
case class CONSTANT_Methodref_info( class_index:Int, name_and_type_index:Int) extends cp_info(ClazzFile.CONSTANT_METHODREF)
case class CONSTANT_InterfaceMethodref_info( class_index:Int, name_and_type_index:Int) extends cp_info(ClazzFile.CONSTANT_INTFMETHODREF)
case class CONSTANT_String_info( string_index:Int) extends cp_info(ClazzFile.CONSTANT_STRING)
case class CONSTANT_Integer_info( bytes:Int) extends cp_info(ClazzFile.CONSTANT_INTEGER)
case class CONSTANT_Float_info(  bytes:Int) extends cp_info(ClazzFile.CONSTANT_FLOAT)
case class CONSTANT_Long_info(  high_bytes:Int, low_bytes:Int ) extends cp_info(ClazzFile.CONSTANT_LONG)
case class CONSTANT_Double_info(  high_bytes:Int, low_bytes:Int)  extends cp_info(ClazzFile.CONSTANT_DOUBLE)
case class CONSTANT_NameAndType_info( name_index:Int, descriptor_index:Int) extends cp_info(ClazzFile.CONSTANT_NAMEANDTYPE)
case class CONSTANT_Utf8_info(  bytes:Array[Byte]) extends cp_info(ClazzFile.CONSTANT_UTF8)
case class CONSTANT_MethodHandle_info( reference_kind:Int, reference_index:Int )  extends cp_info(ClazzFile.CONSTANT_MethodHandle)
case class CONSTANT_MethodType_info( descriptor_index:Int) extends cp_info(ClazzFile.CONSTANT_MethodType)
case class CONSTANT_InvokeDynamic_info( bootstrap_method_attr_index: Int, name_and_type_index: Int) extends cp_info(ClazzFile.CONSTANT_InvokeDynamic)





case class field_info (
  val access_flags:Int,
  val name_index:Int,
  val descriptor_index:Int,
  val attributes:Array[attribute_info]
)

case class method_info (
  val access_flags:Int,
  val name_index:Int,
  val descriptor_index:Int,
  val attributes:Array[attribute_info] 
)

sealed abstract class attribute_info( val name:String) {
  val attribute_name_index =0
//  val info:Array[Byte] = Array.empty
//  def unapply() {Some(name)}
}


case class UnknownAttribute( title:String, info:Array[Byte]) extends attribute_info("*Unknown*")

case class ConstantValue_attribute(constantvalue_index:Int) extends attribute_info(ClazzFile.ATTRIB_ConstantValue)
case class SourceFile_attribute( sourcefile_index:Int ) extends attribute_info(ClazzFile.ATTRIB_SourceFile)
case class InnerClasses_attribute( classes:Array[IC_classes] ) extends attribute_info(ClazzFile.ATTRIB_InnerClasses)
case class RuntimeVisibleAnnotations_attribute( annotations:Array[RVA_annotation]) extends attribute_info(ClazzFile.ATTRIB_RuntimeVisibleAnnotations)
case class Signature_attribute( signature_index:Int ) extends attribute_info(ClazzFile.ATTRIB_Signature)
case class Code_attribute( max_stack:Int, max_locals:Int, code:Array[Byte], exceptions:Array[exception_table], annotations:Array[attribute_info] ) extends attribute_info(ClazzFile.ATTRIB_Code)
case class LineNumberTable_attribute( lines:Array[LNT_Lines]) extends attribute_info(ClazzFile.ATTRIB_LineNumberTable)

//case class ScalaSig_attribute( classes:Array[InnerClasses_attribute_classes] ) extends attribute_info(ClazzFile.ATTRIB_InnerClasses)

// 
case class exception_table( start_pc:Int, end_pc:Int, handler_pc:Int, catch_type:Int)

case class IC_classes(inner_class_info_index:Int, outer_class_info_index:Int, inner_name_index:Int, inner_class_access_flags:Int)


case class LNT_Lines( start_pc:Int, line_number:Int)

case class RVA_annotation( type_index:Int, element_value:Array[RVA_element_value_pairs]  )
case class RVA_element_value_pairs(element_name_index:Int, element_value:Array[RVA_element_value])

// used for RVA_element_value_pairs
sealed class RVA_element_value( tag:Int)
case class RVA_const_value_index(tag:Int, const_value_index:Int ) extends RVA_element_value(tag)
case class RVA_enum_const_value(tag:Int, type_name_index:Int, const_name_index:Int ) extends RVA_element_value(tag)
case class RVA_class_info_index(tag:Int, class_info_index:Int ) extends RVA_element_value(tag)
case class RVA_annotation_value(tag:Int, annotation:RVA_annotation ) extends RVA_element_value(tag)
case class RVA_array_value(tag:Int, values:Array[RVA_element_value] ) extends RVA_element_value(tag)


object ClazzFile {
                                             // Class   Field   Method
  final val JAVA_ACC_PUBLIC       = 0x0001   //   X       X        X
  final val JAVA_ACC_PRIVATE      = 0x0002   //           X        X
  final val JAVA_ACC_PROTECTED    = 0x0004   //           X        X
  final val JAVA_ACC_STATIC       = 0x0008   //           X        X
  final val JAVA_ACC_FINAL        = 0x0010   //   X       X        X
  final val JAVA_ACC_SUPER        = 0x0020   //   X
  final val JAVA_ACC_SYNCHRONIZED = 0x0020   //                    X
  final val JAVA_ACC_VOLATILE     = 0x0040   //           X
  final val JAVA_ACC_BRIDGE       = 0x0040   //                    X
  final val JAVA_ACC_TRANSIENT    = 0x0080   //           X
  final val JAVA_ACC_VARARGS      = 0x0080   //                    X
  final val JAVA_ACC_NATIVE       = 0x0100   //                    X
  final val JAVA_ACC_INTERFACE    = 0x0200   //   X
  final val JAVA_ACC_ABSTRACT     = 0x0400   //   X                X
  final val JAVA_ACC_STRICT       = 0x0800   //                    X
  final val JAVA_ACC_SYNTHETIC    = 0x1000   //   X       X        X
  final val JAVA_ACC_ANNOTATION   = 0x2000   //   X
  final val JAVA_ACC_ENUM         = 0x4000   //   X       X

  
  // tags describing the type of a literal in the constant pool
  final val CONSTANT_DUMMY         =  0  // NOT IN JVM SPEC
  final val CONSTANT_UTF8          =  1
  final val CONSTANT_UNICODE       =  2   // ??
  final val CONSTANT_INTEGER       =  3
  final val CONSTANT_FLOAT         =  4
  final val CONSTANT_LONG          =  5
  final val CONSTANT_DOUBLE        =  6
  final val CONSTANT_CLASS         =  7
  final val CONSTANT_STRING        =  8
  final val CONSTANT_FIELDREF      =  9
  final val CONSTANT_METHODREF     = 10
  final val CONSTANT_INTFMETHODREF = 11
  final val CONSTANT_NAMEANDTYPE   = 12
  final val CONSTANT_MethodHandle  = 15
  final val CONSTANT_MethodType    = 16
  final val CONSTANT_InvokeDynamic = 18

  // tags describing the type of a literal in attribute values
  final val BYTE_TAG   = 'B'
  final val CHAR_TAG   = 'C'
  final val DOUBLE_TAG = 'D'
  final val FLOAT_TAG  = 'F'
  final val INT_TAG    = 'I'
  final val LONG_TAG   = 'J'
  final val SHORT_TAG  = 'S'
  final val BOOL_TAG   = 'Z'
  final val STRING_TAG = 's'
  final val ENUM_TAG   = 'e'
  final val CLASS_TAG  = 'c'
  final val ARRAY_TAG  = '['
  final val VOID_TAG   = 'V'
  final val TVAR_TAG   = 'T'
  final val ANNOTATION_TAG = '@'
  final val SCALA_NOTHING = "scala.runtime.Nothing$"
  final val SCALA_NULL = "scala.runtime.Null$"


  // tags describing the type of newarray
  final val T_BOOLEAN = 4
  final val T_CHAR    = 5
  final val T_FLOAT   = 6
  final val T_DOUBLE  = 7
  final val T_BYTE    = 8
  final val T_SHORT   = 9
  final val T_INT     = 10
  final val T_LONG    = 11
  
  
  final val ATTRIB_ConstantValue = "ConstantValue"
  final val ATTRIB_Code = "Code"
  final val ATTRIB_StackMapTable = "StackMapTable"
  final val ATTRIB_Exceptions = "Exceptions"
  final val ATTRIB_InnerClasses = "InnerClasses"
  final val ATTRIB_EnclosingMethod = "EnclosingMethod"
  final val ATTRIB_Synthetic = "Synthetic"
  final val ATTRIB_Signature = "Signature"
  final val ATTRIB_SourceFile = "SourceFile"
  final val ATTRIB_SourceDebugExtension = "SourceDebugExtension"
  final val ATTRIB_LineNumberTable = "LineNumberTable"
  final val ATTRIB_LocalVariableTable = "LocalVariableTable"
  final val ATTRIB_LocalVariableTypeTable = "LocalVariableTypeTable"
  final val ATTRIB_Deprecated = "Deprecated"
  final val ATTRIB_RuntimeVisibleAnnotations = "RuntimeVisibleAnnotations"
  final val ATTRIB_RuntimeInvisibleAnnotations = "RuntimeInvisibleAnnotations"
  final val ATTRIB_RuntimeVisibleParameterAnnotations = "RuntimeVisibleParameterAnnotations"
  final val ATTRIB_RuntimeInvisibleParameterAnnotations = "RuntimeInvisibleParameterAnnotations"
  final val ATTRIB_AnnotationDefault = "AnnotationDefault"
  final val ATTRIB_Scala = "Scala"
  final val ATTRIB_ScalaSig = "ScalaSig"
    
  
}