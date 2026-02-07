cmake_minimum_required(VERSION 4.2)
project(mal)

set(CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(APPLE)
  execute_process(
    COMMAND brew --prefix
    OUTPUT_VARIABLE BREW_PREFIX
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  if(BREW_PREFIX)
    list(APPEND CMAKE_PREFIX_PATH "${BREW_PREFIX}/opt/readline")
  endif()
endif()

find_package(PkgConfig REQUIRED)
pkg_check_modules(READLINE readline)

if(NOT READLINE_FOUND)
    message(FATAL_ERROR "Readline not found!
    Linux: sudo apt install libreadline-dev
    macOS: brew install readline")
endif()

add_library(mal ReadLine.cpp)
target_include_directories(mal PUBLIC ${READLINE_INCLUDE_DIRS})
target_link_libraries(mal INTERFACE ${READLINE_LIBRARIES})
target_link_directories(mal INTERFACE ${READLINE_LIBRARY_DIRS})
file(GLOB mains step*.cpp)

foreach(main ${mains})
  get_filename_component(target ${main} NAME_WLE)
  add_executable(${target} ${main})
  target_link_libraries(${target} PRIVATE mal)
endforeach()
