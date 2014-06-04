add_llvm_library(LLVMDuettoUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  RemovePHI.cpp
  ResolveAliases.cpp
  ReplaceNopCasts.cpp
  )

add_dependencies(LLVMDuettoUtils intrinsics_gen)
