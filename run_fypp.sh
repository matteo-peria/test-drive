#!/bin/zsh

fypp ./src/testdrive_multidim_interface.fypp > src/testdrive_multidim_interface.inc
#fypp ./src/testdrive_multidim_stubs.fypp > src/testdrive_multidim_stubs.inc
## submodule bodies:
##fypp ./src/testdrive_multidim.fypp           > src/testdrive_multidim.F90
fypp ./src/testdrive_multidim.fypp > src/testdrive_multidim.inc

