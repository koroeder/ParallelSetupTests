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

### CPU jobs and hyperthreading
The code runs one job per **physical core** available to the job (counted from the process affinity mask and
`/sys/devices/system/cpu/cpuN/topology/thread_siblings_list`), not per `SLURM_NTASKS`.
Each job is launched as `srun -n1 -N1 -c<CPUs per core> --exact --cpu-bind=cores --mem-per-cpu=<MB>`
(`--exclusive` instead of `--exact` for Slurm < 20.11).

On nodes with 2 threads per core, `--ntasks=N` may only give N hardware threads = N/2 physical cores, and hence N/2 concurrent jobs.
To run N jobs concurrently, request N physical cores, e.g.:

    #SBATCH --ntasks=N
    #SBATCH --ntasks-per-core=1

(alternatively `--hint=nomultithread`). Check the start of the output: it reports hardware threads, physical cores,
`SLURM_NTASKS`, `SLURM_CPUS_ON_NODE` and the resulting number of slots, and warns when these disagree.
Memory per step is taken from `SLURM_MEM_PER_CPU`, or `SLURM_MEM_PER_NODE / SLURM_CPUS_ON_NODE`; request memory explicitly in the job.

### Multi-node allocations
With `SLURM_JOB_NUM_NODES > 1` the slot table covers all nodes: node names from `scontrol show hostnames $SLURM_JOB_NODELIST`,
CPUs per node from `SLURM_JOB_CPUS_PER_NODE`, `ThreadsPerCore` per node from `scontrol show node`; slots per node = CPUs / ThreadsPerCore.
Slots are assigned round-robin over nodes and each job is placed with `srun ... -w <node>`.
The submit directory must be on a filesystem shared by all nodes (a warning is printed if it looks node-local).
All srun client processes run on the first node, one per running job.

### GPU jobs and MPS
    $PathToBin/ParallelTest njobs gpu [P] [nomps]

*P* = jobs per GPU (default 1). Slots per node = min(physical cores, GPUs per node x P); GPUs per node from
`SLURM_GPUS_ON_NODE` (fallback: entries in `CUDA_VISIBLE_DEVICES`), the same on all nodes.

- P = 1: each job step requests `--gres=gpu:1` and Slurm assigns the GPU.
- P > 1: job steps are launched with `srun --overlap -c<all job CPUs of the node>` and no `--gres`.
  A step without `--gres` gets all job GPUs exclusively, so without `--overlap` such steps run one at a time.
  With `--overlap`, Slurm would put every step on the same core, so each job script first pins itself to the
  physical core of its slot (`pin_core.sh` + `taskset`). It then checks that the step sees the job's GPUs and sets
  `CUDA_VISIBLE_DEVICES` to the GPU of its slot (round-robin). By default the GPUs are shared via **MPS**:
  one control daemon per node runs in its own `srun --overlap` step (`mps_daemon.sh`) with pipe/log
  directories in `/dev/shm/mps_$SLURM_JOB_ID` (`PARALLELTEST_MPS_BASE` overrides `/dev/shm`). `/tmp` cannot be used:
  on this cluster every step has a private `/tmp` (SPANK `private-tmpdir`). Readiness and shutdown are signalled
  through `<submitdir>/.mps_$SLURM_JOB_ID/`, where the control logs are kept. `nomps` shares the GPUs by time-slicing instead.
- The run stops with an error if a daemon fails to start or ends early. A job exits with status 3 if its step sees no GPU,
  4 if it cannot reach the MPS daemon (it prints the client's message and the pipe directory listing), and 6 if it cannot
  pin itself to its core.
- `PARALLELTEST_GPU_STEP_FLAGS` replaces the default step flags (`--overlap`) for P > 1, for testing.
- `PARALLELTEST_JOB_CMD` appends a command (e.g. a `pmemd.cuda` call) to every job script; the job exits with status 5 if it fails.

Validation on the cluster: `tests/validate_gpu_mps.sbatch`.

## Run-time checks
The code executes a simple chain of creating a small submit script for srun and running that script as child process.
Each srun step waits for some time, then creates a file in a new folder, and then waits again.
After this the job is done, and replaced with a new one

### Expected behaviour
Using *sacct* on the job submitted should show the resource allocations. The setup should run *ncores* (physical cores) jobs on 1 core each plus two processes on all cores (the overall job allocation).
Each job prints a line `job <n> host=<node> step=<id> Cpus_allowed_list: <cpus>`; concurrently running jobs must show disjoint core (sibling) pairs.
The job output must not contain "Step creation temporarily disabled".
If this is not the case, something is wrong with the setup.
