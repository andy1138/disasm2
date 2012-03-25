package com.scalalabs.disasm.format

import com.scalalabs.disasm.classfile.ClazzFile

trait NullFormat extends FormatRepositoryComponent {
  def formatter = new NullFormatImpl
  
  class NullFormatImpl extends FormatRepository {
    def format(clazzFile:ClazzFile) = "*NullFormatImpl*" :: Nil
  }
} 
