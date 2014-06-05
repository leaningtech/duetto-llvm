add_llvm_library(LLVMDuettoWriter
  
  DuettoWriter.cpp
  GlobalDepsAnalyzer.cpp
  Inliner.cpp
  JSInterop.cpp
  Meminst.cpp
  NameGenerator.cpp
  Opcodes.cpp
  PointerAnalyzer.cpp
  Relooper.cpp
  SourceMaps.cpp
  Types.cpp
  Utility.cpp
)

add_dependencies(LLVMDuettoWriter intrinsics_gen)
