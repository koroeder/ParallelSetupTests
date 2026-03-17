cmake_minimum_required(VERSION 3.10)
project(TestParallelSetup)

# Enable Fortran
enable_language(Fortran)

# Set compiler flags
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0")
else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
endif()

# List of Fortran source files
set(FORTRAN_SOURCES
       	main.f90
	mod_hpc_environ.f90
	create_job_string.f90
)

# Add the executable
add_executable(ParallelTest ${FORTRAN_SOURCES})
