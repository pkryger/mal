cmake_minimum_required(VERSION 4.2)
project(mal LANGUAGES CXX)

if(APPLE)
  execute_process(
    COMMAND brew --prefix readline
    OUTPUT_VARIABLE READLINE_PREFIX
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(READLINE_PREFIX)
    list(APPEND CMAKE_PREFIX_PATH "${READLINE_PREFIX}")
  endif()
  execute_process(
    COMMAND brew --prefix llvm
    OUTPUT_VARIABLE LLVM_PREFIX
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(LLVM_PREFIX)
    list(APPEND CMAKE_PREFIX_PATH "${LLVM_PREFIX}")
  endif()
endif()

find_program(CLANG_TIDY clang-tidy)
if(CLANG_TIDY)
  set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY})
endif()

find_package(PkgConfig REQUIRED)
pkg_check_modules(READLINE readline)

if(NOT READLINE_FOUND)
    message(FATAL_ERROR "Readline not found!
    Linux: sudo apt install libreadline-dev
    macOS: brew install readline")
endif()


add_library(project_sanitizers INTERFACE)
target_compile_options(project_sanitizers INTERFACE
  -fsanitize=address,undefined
  -fno-omit-frame-pointer
)
target_link_options(project_sanitizers INTERFACE
    -fsanitize=address,undefined
)

add_library(mal)
target_sources(mal
  PUBLIC
    FILE_SET HEADERS
    FILES
      Core.h
      Env.h
      FunctionRef.h
      GarbageCollector.h
      Hierarchy.h
      InPlaceAllocator.h
      Mal.h
      Ranges.h
      Reader.h
      ReadLine.h
      Specials.h
      StringEscaping.h
      Types.h
  PRIVATE
    Core.cpp
    Env.cpp
    ReadLine.cpp
    Reader.cpp
    Specials.cpp
    StringEscaping.cpp
    Types.cpp
)
target_include_directories(mal PUBLIC ${READLINE_INCLUDE_DIRS})
target_link_libraries(mal INTERFACE
  ${READLINE_LIBRARIES}
  $<$<CONFIG:Debug>:project_sanitizers>
)
target_link_directories(mal INTERFACE ${READLINE_LIBRARY_DIRS})
target_compile_features(mal PUBLIC cxx_std_23)
target_compile_options(mal PRIVATE
  $<$<CXX_COMPILER_ID:GNU,Clang,AppleClang>:
    -Wall -Wextra -Wconversion -Wshadow
   >
  $<$<CXX_COMPILER_ID:MSVC>:
    /W4
   >
)
set_target_properties(mal
  PROPERTIES VERIFY_INTERFACE_HEADER_SETS ON)

file(GLOB mains CONFIGURE_DEPENDS step*.cpp)
foreach(main ${mains})
  get_filename_component(target ${main} NAME_WLE)
  add_executable(${target} ${main})
  target_link_libraries(${target} PRIVATE
    mal
    $<$<CONFIG:Debug>:project_sanitizers>
  )
endforeach()

if(CMAKE_EXPORT_COMPILE_COMMANDS)
  cmake_path(RELATIVE_PATH CMAKE_CURRENT_BINARY_DIR
    BASE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE relbuilddir)
  if(NOT "${relbuilddir}" MATCHES "^\\.\\./" AND
    "${relbuilddir}" MATCHES "^build" AND
    NOT "${relbuilddir}" STREQUAL "build")
    cmake_path(APPEND CMAKE_CURRENT_BINARY_DIR compile_commands.json
      OUTPUT_VARIABLE target)
    cmake_path(APPEND CMAKE_CURRENT_SOURCE_DIR build compile_commands.json
      OUTPUT_VARIABLE linkname)
    file(REMOVE "${linkname}")
    file(CREATE_LINK "${target}" "${linkname}" SYMBOLIC)
  endif()
endif()
