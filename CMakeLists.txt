cmake_minimum_required(VERSION 2.6)
project(CppQuickCheck)

##### Boost #####

set (Boost_FIND_REQUIRED TRUE)
set (Boost_FIND_QUIETLY TRUE)
set (Boost_DEBUG FALSE)
set (Boost_USE_MULTITHREADED TRUE)
set (Boost_USE_STATIC_LIBS TRUE)

# Expand the next statement if newer boost versions than 1.47.0 are released
set(Boost_ADDITIONAL_VERSIONS 1.40 1.41.0 1.42.0 1.43.0 1.44.0 1.45.0 1.46.0 1.47.0 1.48.0 1.49.0 1.50.0)

find_package(Boost 1.40 REQUIRED system)

include_directories("${PROJECT_SOURCE_DIR}/include" ${Boost_INCLUDE_DIRS})

link_directories(${Boost_LIBRARY_DIRS})

add_subdirectory(examples)

add_library(cppqc STATIC src/Arbitrary.cpp)

install(DIRECTORY "include/" DESTINATION "include"
    PATTERN ".*" EXCLUDE)
install(TARGETS cppqc DESTINATION "lib")
