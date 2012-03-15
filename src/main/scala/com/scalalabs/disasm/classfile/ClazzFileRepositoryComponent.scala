package com.scalalabs.disasm.classfile

trait ClazzFileRepositoryComponent {
  def clazzFile: ClassFileRepository
  
  trait ClassFileRepository {
    def parse(): ClazzFile
    def createResult(): Unit
  }
}
