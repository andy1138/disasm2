package com.scalalabs.disasm.format
import com.scalalabs.disasm.classfile.ClazzFile

trait FormatRepositoryComponent {
  def formatter: FormatRepository
  
  trait FormatRepository {
    def format(clazzFile:ClazzFile):List[String]
  }
}
