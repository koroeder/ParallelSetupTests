# ParallelSetupTests

To test the PATHSAMPLE style setup for jobs running a fork and wait scheme from within a job to use srun asynchronously.

## Compilation
Create new build directory and load/install cmake, fortran and C compiler (tested for GNU compiler)

Enter build dir and run:
FC=gfortran CC=gcc cmake $PathToSrc

Once completed, run *make* to compile.

## Run job
Submit a job to the cluster queue as normal with the appropriate time and resource allocations
Run $PathToBin/ParallelTest *njobs* >& output

*njobs* should be larger than *ncores* to see how fork and wait procedures run.

## Run-time checks
The code executes a simple chain of creating a small submit script for srun and running that script as child process.
Each srun step waits for some time, then creates a file in a new folder, and then waits again.
After this the job is done, and replaced with a new one

### Expected behaviour
Using *sacct* on the job submitted should show the resource allocations. The setup should run *ncores* jobs on 1 core each plus two processes on all cores (the overall job allocation).
If this is not the case, something is wrong with the setup.
