# Build script
rm -f priorityQueueModule.mod
gfortran -c priorityQueueModule.f90 -O3
gfortran -c priorityQueue.f90 -O3
gfortran priorityQueueModule.o priorityQueue.o -o prioQueueFortran -O3