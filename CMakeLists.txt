cmake_minimum_required(VERSION 3.1.0)

# enable FORTRAN language
enable_language(Fortran)

# project name declaration
project(AIRFOIL_generator)

# checking needed program: GNUplot
find_package(Gnuplot REQUIRED)

# setting up output files directories
#     binaries: 
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
#     .mod files:
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

# source files' position declaration
file(GLOB_RECURSE source_files ${PROJECT_SOURCE_DIR}/src/*.f90)
file(GLOB_RECURSE module_files ${PROJECT_SOURCE_DIR}/src/modules/*.f90)

# making executable
add_executable(agen ${source_files} ${module_files})

# coping .plt files
file(COPY ${PROJECT_SOURCE_DIR}/src/plt/AIRFOILgnuplot.plt      DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/bin)
file(COPY ${PROJECT_SOURCE_DIR}/src/plt/AIRFOILelement_plot.plt DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/bin)
file(COPY ${PROJECT_SOURCE_DIR}/src/plt/MEANplot.plt            DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/bin)
file(COPY ${PROJECT_SOURCE_DIR}/src/plt/COORDSplot.plt          DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/bin)