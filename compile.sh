#! /bin/zsh

# Compile rust library with cargo
cd build/rust_utils
cargo build
cbindgen --config cbindgen.toml --crate rust_utils --lang c --output ../include/r_utils.h

# Go back to root dir
cd ../../

# Compile with cmake
cmake build/
cmake --build .
