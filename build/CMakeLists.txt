# Skeleton from : https://github.com/ttroy50/cmake-examples/blob/master/01-basic/B-hello-headers/CMakeLists.txt
# Set the minimum version of CMake that can be used
# To find the cmake version run
# $ cmake --version
cmake_minimum_required(VERSION 3.2)

# Set the project name
project (gb)

# Create a sources variable with a link to all cpp files to compile
set(SOURCES
    src/main.c
    src/mem/mem.c
    src/cpu/cpu.c
    src/instruction/instruction.c
    src/utils/utils.c
    include/r_utils.h
    )

# Add an executable with the above sources
add_compile_options(-g)
add_executable(gb ${SOURCES})

# Specify Example's link libraries
target_link_libraries(gb
    "${CMAKE_SOURCE_DIR}/rust_utils/target/debug/libr_utils.a"
    )
# Set the directories that should be included in the build command for this target
# when running g++ these will be included as -I/directory/path/
target_include_directories(gb
    PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    )

# GoogleTest requires at least C++11
set(CMAKE_CXX_STANDARD 11)

include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/609281088cfefc76f9d0ce82e1ff6c30cc3591e5.zip
    )
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

# Testing memory 
add_executable(test_memory testing/mem_test.cpp src/mem/mem.c src/utils/utils.c)

add_test(testAll test_memory)

target_link_libraries(test_memory gtest_main)

# Testing utils
add_executable(test_utils testing/utils_test.cpp src/utils/utils.c )

add_test(testAll test_utils)

target_link_libraries(test_utils gtest_main)

set(TEST_SOURCES
    src/mem/mem.c
    src/cpu/cpu.c
    src/instruction/instruction.c
    src/utils/utils.c
    include/r_utils.h
    )

# Testing cpu
add_executable(test_cpu testing/cpu_test.cpp ${TEST_SOURCES} )

# Specify Example's link libraries
target_link_libraries(test_cpu
    "${CMAKE_SOURCE_DIR}/rust_utils/target/debug/libr_utils.a"
    )

add_test(testAll test_cpu)

target_link_libraries(test_cpu gtest_main)

# Testing instruction
add_executable(test_instruction testing/instruction_test.cpp src/instruction/instruction.c src/utils/utils.c  include/r_utils.h src/mem/mem.c src/cpu/cpu.c)

add_compile_options(test_instruction -g)

add_test(testAll test_instruction)

target_link_libraries(test_instruction gtest_main)
# Specify Example's link libraries
target_link_libraries(test_instruction
    "${CMAKE_SOURCE_DIR}/rust_utils/target/debug/libr_utils.a"
    )
