function cmake-llvm
    cmake -S llvm -B build -G Ninja -DLLVM_ENABLE_PROJECTS="clang;openmp" -DCMAKE_BUILD_TYPE=Debug -DLLVM_CCACHE_BUILD=ON
end
