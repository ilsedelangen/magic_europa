# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/delangen/magic

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/delangen/magic/build

# Include any dependencies generated for this target.
include src/CMakeFiles/lapack95.dir/depend.make

# Include the progress variables for this target.
include src/CMakeFiles/lapack95.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/lapack95.dir/flags.make

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o: src/CMakeFiles/lapack95.dir/flags.make
src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o: /opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/delangen/magic/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o"
	cd /home/delangen/magic/build/src && /opt/local/mpi/openmpi/openmpi-2.1.2_intel-2019.4/bin/mpif90  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90 -o CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.i"
	cd /home/delangen/magic/build/src && /opt/local/mpi/openmpi/openmpi-2.1.2_intel-2019.4/bin/mpif90  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90 > CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.i

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.s"
	cd /home/delangen/magic/build/src && /opt/local/mpi/openmpi/openmpi-2.1.2_intel-2019.4/bin/mpif90  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90 -o CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.s

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.requires:

.PHONY : src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.requires

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.provides: src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.requires
	$(MAKE) -f src/CMakeFiles/lapack95.dir/build.make src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.provides.build
.PHONY : src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.provides

src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.provides.build: src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o


# Object files for target lapack95
lapack95_OBJECTS = \
"CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o"

# External object files for target lapack95
lapack95_EXTERNAL_OBJECTS =

src/liblapack95.a: src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o
src/liblapack95.a: src/CMakeFiles/lapack95.dir/build.make
src/liblapack95.a: src/CMakeFiles/lapack95.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/delangen/magic/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran static library liblapack95.a"
	cd /home/delangen/magic/build/src && $(CMAKE_COMMAND) -P CMakeFiles/lapack95.dir/cmake_clean_target.cmake
	cd /home/delangen/magic/build/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/lapack95.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/lapack95.dir/build: src/liblapack95.a

.PHONY : src/CMakeFiles/lapack95.dir/build

src/CMakeFiles/lapack95.dir/requires: src/CMakeFiles/lapack95.dir/opt/local/intel/ps_xe-2019.4/compilers_and_libraries_2019.4.243/linux/mkl/include/lapack.f90.o.requires

.PHONY : src/CMakeFiles/lapack95.dir/requires

src/CMakeFiles/lapack95.dir/clean:
	cd /home/delangen/magic/build/src && $(CMAKE_COMMAND) -P CMakeFiles/lapack95.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/lapack95.dir/clean

src/CMakeFiles/lapack95.dir/depend:
	cd /home/delangen/magic/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/delangen/magic /home/delangen/magic/src /home/delangen/magic/build /home/delangen/magic/build/src /home/delangen/magic/build/src/CMakeFiles/lapack95.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/lapack95.dir/depend
