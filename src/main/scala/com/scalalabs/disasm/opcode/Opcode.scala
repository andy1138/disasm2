/**
 * Disassembler
 * (c)2012 andy hicks 
 */

package com.scalalabs.disasm.opcode

object Opcode {

  object Kind extends Enumeration {
  val NoOperands = Value
  val AType = Value
  val Branch = Value
  val BranchW = Value
  val Byte = Value
  val CPRef = Value
  val CPRefW = Value
  val CPRefWUByte = Value
  val CPRefWUByteZero = Value
  val Dynamic = Value
  val Local = Value
  val LocalByte = Value
  val Short = Value
  val WideNoOperands = Value
  val WideCPRefW = Value
  val WideCPRefWShort = Value
  val Unknown = Value
  
  }
  
  import Kind._
  val opcode = Array[(String, Int, Kind.Value)] (
  ("nop", 0x00, NoOperands),
    ("aconst_null", 0x1, NoOperands),
    ("iconst_m1", 0x2, NoOperands),
    ("iconst_0", 0x3, NoOperands),
    ("iconst_1", 0x4, NoOperands),
    ("iconst_2", 0x5, NoOperands),
    ("iconst_3", 0x6, NoOperands),
    ("iconst_4", 0x7, NoOperands),
    ("iconst_5", 0x8, NoOperands),
    ("lconst_0", 0x9, NoOperands),
    ("lconst_1", 0xa, NoOperands),
    ("fconst_0", 0xb, NoOperands),
    ("fconst_1", 0xc, NoOperands),
    ("fconst_2", 0xd, NoOperands),
    ("dconst_0", 0xe, NoOperands),
    ("dconst_1", 0xf, NoOperands),

    ("bipush", 0x10, Byte),
    ("sipush", 0x11, Short),
    ("ldc", 0x12, CPRef),
    ("ldc_w", 0x13, CPRefW),
    ("ldc2_w", 0x14, CPRefW),
    ("iload", 0x15, Local),
    ("lload", 0x16, Local),
    ("fload", 0x17, Local),
    ("dload", 0x18, Local),
    ("aload", 0x19, Local),
    ("iload_0", 0x1a, NoOperands),
    ("iload_1", 0x1b, NoOperands),
    ("iload_2", 0x1c, NoOperands),
    ("iload_3", 0x1d, NoOperands),
    ("lload_0", 0x1e, NoOperands),
    ("lload_1", 0x1f, NoOperands),

    ("lload_2", 0x20, NoOperands),
    ("lload_3", 0x21, NoOperands),
    ("fload_0", 0x22, NoOperands),
    ("fload_1", 0x23, NoOperands),
    ("fload_2", 0x24, NoOperands),
    ("fload_3", 0x25, NoOperands),
    ("dload_0", 0x26, NoOperands),
    ("dload_1", 0x27, NoOperands),
    ("dload_2", 0x28, NoOperands),
    ("dload_3", 0x29, NoOperands),
    ("aload_0", 0x2a, NoOperands),
    ("aload_1", 0x2b, NoOperands),
    ("aload_2", 0x2c, NoOperands),
    ("aload_3", 0x2d, NoOperands),
    ("iaload", 0x2e, NoOperands),
    ("laload", 0x2f, NoOperands),

    ("faload", 0x30, NoOperands),
    ("daload", 0x31, NoOperands),
    ("aaload", 0x32, NoOperands),
    ("baload", 0x33, NoOperands),
    ("caload", 0x34, NoOperands),
    ("saload", 0x35, NoOperands),
    ("istore", 0x36, Local),
    ("lstore", 0x37, Local),
    ("fstore", 0x38, Local),
    ("dstore", 0x39, Local),
    ("astore", 0x3a, Local),
    ("istore_0", 0x3b, NoOperands),
    ("istore_1", 0x3c, NoOperands),
    ("istore_2", 0x3d, NoOperands),
    ("istore_3", 0x3e, NoOperands),
    ("lstore_0", 0x3f, NoOperands),

    ("lstore_1", 0x40, NoOperands),
    ("lstore_2", 0x41, NoOperands),
    ("lstore_3", 0x42, NoOperands),
    ("fstore_0", 0x43, NoOperands),
    ("fstore_1", 0x44, NoOperands),
    ("fstore_2", 0x45, NoOperands),
    ("fstore_3", 0x46, NoOperands),
    ("dstore_0", 0x47, NoOperands),
    ("dstore_1", 0x48, NoOperands),
    ("dstore_2", 0x49, NoOperands),
    ("dstore_3", 0x4a, NoOperands),
    ("astore_0", 0x4b, NoOperands),
    ("astore_1", 0x4c, NoOperands),
    ("astore_2", 0x4d, NoOperands),
    ("astore_3", 0x4e, NoOperands),
    ("iastore", 0x4f, NoOperands),

    ("lastore", 0x50, NoOperands),
    ("fastore", 0x51, NoOperands),
    ("dastore", 0x52, NoOperands),
    ("aastore", 0x53, NoOperands),
    ("bastore", 0x54, NoOperands),
    ("castore", 0x55, NoOperands),
    ("sastore", 0x56, NoOperands),
    ("pop", 0x57, NoOperands),
    ("pop2", 0x58, NoOperands),
    ("dup", 0x59, NoOperands),
    ("dup_x1", 0x5a, NoOperands),
    ("dup_x2", 0x5b, NoOperands),
    ("dup2", 0x5c, NoOperands),
    ("dup2_x1", 0x5d, NoOperands),
    ("dup2_x2", 0x5e, NoOperands),
    ("swap", 0x5f, NoOperands),

    ("iadd", 0x60, NoOperands),
    ("ladd", 0x61, NoOperands),
    ("fadd", 0x62, NoOperands),
    ("dadd", 0x63, NoOperands),
    ("isub", 0x64, NoOperands),
    ("lsub", 0x65, NoOperands),
    ("fsub", 0x66, NoOperands),
    ("dsub", 0x67, NoOperands),
    ("imul", 0x68, NoOperands),
    ("lmul", 0x69, NoOperands),
    ("fmul", 0x6a, NoOperands),
    ("dmul", 0x6b, NoOperands),
    ("idiv", 0x6c, NoOperands),
    ("ldiv", 0x6d, NoOperands),
    ("fdiv", 0x6e, NoOperands),
    ("ddiv", 0x6f, NoOperands),

    ("irem", 0x70, NoOperands),
    ("lrem", 0x71, NoOperands),
    ("frem", 0x72, NoOperands),
    ("drem", 0x73, NoOperands),
    ("ineg", 0x74, NoOperands),
    ("lneg", 0x75, NoOperands),
    ("fneg", 0x76, NoOperands),
    ("dneg", 0x77, NoOperands),
    ("ishl", 0x78, NoOperands),
    ("lshl", 0x79, NoOperands),
    ("ishr", 0x7a, NoOperands),
    ("lshr", 0x7b, NoOperands),
    ("iushr", 0x7c, NoOperands),
    ("lushr", 0x7d, NoOperands),
    ("iand", 0x7e, NoOperands),
    ("land", 0x7f, NoOperands),

    ("ior", 0x80, NoOperands),
    ("lor", 0x81, NoOperands),
    ("ixor", 0x82, NoOperands),
    ("lxor", 0x83, NoOperands),
    ("iinc", 0x84, LocalByte),
    ("i2l", 0x85, NoOperands),
    ("i2f", 0x86, NoOperands),
    ("i2d", 0x87, NoOperands),
    ("l2i", 0x88, NoOperands),
    ("l2f", 0x89, NoOperands),
    ("l2d", 0x8a, NoOperands),
    ("f2i", 0x8b, NoOperands),
    ("f2l", 0x8c, NoOperands),
    ("f2d", 0x8d, NoOperands),
    ("d2i", 0x8e, NoOperands),
    ("d2l", 0x8f, NoOperands),

    ("d2f", 0x90, NoOperands),
    ("i2b", 0x91, NoOperands),
    ("i2c", 0x92, NoOperands),
    ("i2s", 0x93, NoOperands),
    ("lcmp", 0x94, NoOperands),
    ("fcmpl", 0x95, NoOperands),
    ("fcmpg", 0x96, NoOperands),
    ("dcmpl", 0x97, NoOperands),
    ("dcmpg", 0x98, NoOperands),
    ("ifeq", 0x99, Branch),
    ("ifne", 0x9a, Branch),
    ("iflt", 0x9b, Branch),
    ("ifge", 0x9c, Branch),
    ("ifgt", 0x9d, Branch),
    ("ifle", 0x9e, Branch),
    ("if_icmpeq", 0x9f, Branch),

    ("if_icmpne", 0xa0, Branch),
    ("if_icmplt", 0xa1, Branch),
    ("if_icmpge", 0xa2, Branch),
    ("if_icmpgt", 0xa3, Branch),
    ("if_icmple", 0xa4, Branch),
    ("if_acmpeq", 0xa5, Branch),
    ("if_acmpne", 0xa6, Branch),
    ("goto", 0xa7, Branch),
    ("jsr", 0xa8, Branch),
    ("ret", 0xa9, Local),
    ("tableswitch", 0xaa, Dynamic),
    ("lookupswitch", 0xab, Dynamic),
    ("ireturn", 0xac, NoOperands),
    ("lreturn", 0xad, NoOperands),
    ("freturn", 0xae, NoOperands),
    ("dreturn", 0xaf, NoOperands),
    
    ("areturn", 0xb0, NoOperands),
    ("return", 0xb1, NoOperands),
    ("getstatic", 0xb2, CPRefW),
    ("putstatic", 0xb3, CPRefW),
    ("getfield", 0xb4, CPRefW),
    ("putfield", 0xb5, CPRefW),
    ("invokevirtual", 0xb6, CPRefW),
    ("invokespecial", 0xb7, CPRefW),
    ("invokestatic", 0xb8, CPRefW),
    ("invokeinterface", 0xb9, CPRefWUByteZero),
    ("invokedynamic", 0xba, CPRefWUByteZero),
    ("new", 0xbb, CPRefW),
    ("newarray", 0xbc, AType),
    ("anewarray", 0xbd, CPRefW),
    ("arraylength", 0xbe, NoOperands),
    ("athrow", 0xbf, NoOperands),
    
    ("checkcast", 0xc0, CPRefW),
    ("instanceof", 0xc1, CPRefW),
    ("monitorenter", 0xc2, NoOperands),
    ("monitorexit", 0xc3, NoOperands),
    ("not used", 0xc4, NoOperands),
    // wide 0xc4
    ("multianewarray", 0xc5, CPRefWUByte),
    ("ifnull", 0xc6, Branch),
    ("ifnonnull", 0xc7, Branch),
    ("goto_w", 0xc8, BranchW),
    ("jsr_w", 0xc9, BranchW)
    // impdep 0xfe: PicoJava nonpriv
    // impdep 0xff: Picojava priv

    // wide opcodes
//    ILOAD_W(0xc415, WIDE_CPREF_W),
//    LLOAD_W(0xc416, WIDE_CPREF_W),
//    FLOAD_W(0xc417, WIDE_CPREF_W),
//    DLOAD_W(0xc418, WIDE_CPREF_W),
//    ALOAD_W(0xc419, WIDE_CPREF_W),
//    ISTORE_W(0xc436, WIDE_CPREF_W),
//    LSTORE_W(0xc437, WIDE_CPREF_W),
//    FSTORE_W(0xc438, WIDE_CPREF_W),
//    DSTORE_W(0xc439, WIDE_CPREF_W),
//    ASTORE_W(0xc43a, WIDE_CPREF_W),
//    IINC_W(0xc484, WIDE_CPREF_W_SHORT),
//    RET_W(0xc4a9, WIDE_CPREF_W),
  )
  
  
  val opsize = Map[Value, Int](
  NoOperands -> 0,
  AType -> 1,
  Branch -> 2,
  BranchW -> 4,
  Byte -> 1,
  CPRef -> 1,
  CPRefW -> 2,
  CPRefWUByte -> 3,
  CPRefWUByteZero -> 4,
  Dynamic -> 0,
  Local -> 1,
  LocalByte -> 1,
  Short -> 2,
  WideNoOperands -> 0,
  WideCPRefW -> 2,
  WideCPRefWShort -> 2,
  Unknown -> 0
  )
  
}

