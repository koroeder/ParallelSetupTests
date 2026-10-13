#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/**
    Counts the hardware threads and physical cores this process may run on.

    The affinity mask (which respects the Slurm cgroup cpuset) gives the
    allowed hardware threads. Each thread is mapped to its physical core via
    the lowest CPU in /sys/devices/system/cpu/cpuN/topology/thread_siblings_list,
    so hyperthread siblings are counted as one core.

    @param nthreads Number of hardware threads in the affinity mask.
    @param ncores   Number of distinct physical cores in the affinity mask.
    @return 0 on success, -1 on failure.
 */
int get_cpu_topology(int* nthreads, int* ncores)
{
    // Number of CPUs configured on the node, used to size the mask.
    long ncpu = sysconf(_SC_NPROCESSORS_CONF);
    cpu_set_t* mask;
    size_t masksize;
    // Marks physical cores (by lowest sibling id) that were already counted.
    char* seen;
    char path[128];
    FILE* f;
    int first;
    long cpu;

    *nthreads = 0;
    *ncores = 0;
    if (ncpu < 1) {
        fprintf(stderr, "get_cpu_topology: cannot determine number of CPUs\n");
        return -1;
    }

    mask = CPU_ALLOC(ncpu);
    if (!mask) {
        perror("get_cpu_topology: CPU_ALLOC failed");
        return -1;
    }
    masksize = CPU_ALLOC_SIZE(ncpu);
    CPU_ZERO_S(masksize, mask);
    if (sched_getaffinity(0, masksize, mask) != 0) {
        perror("get_cpu_topology: sched_getaffinity failed");
        CPU_FREE(mask);
        return -1;
    }

    seen = (char*)calloc(ncpu, 1);
    if (!seen) {
        perror("get_cpu_topology: calloc failed");
        CPU_FREE(mask);
        return -1;
    }

    for (cpu = 0; cpu < ncpu; ++cpu) {
        if (!CPU_ISSET_S(cpu, masksize, mask)) continue;
        ++(*nthreads);
        // The sibling list starts with the lowest CPU id of this core, e.g.
        // "0,64" or "0-1". If it cannot be read, treat the thread as a core.
        first = (int)cpu;
        snprintf(path, sizeof(path),
                 "/sys/devices/system/cpu/cpu%ld/topology/thread_siblings_list", cpu);
        f = fopen(path, "r");
        if (f) {
            if (fscanf(f, "%d", &first) != 1) first = (int)cpu;
            fclose(f);
        }
        if (first < 0 || first >= ncpu) first = (int)cpu;
        if (!seen[first]) {
            seen[first] = 1;
            ++(*ncores);
        }
    }

    free(seen);
    CPU_FREE(mask);
    return 0;
}
