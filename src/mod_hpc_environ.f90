module HPCenvironment
   implicit none
   character(len=100) :: hostname
   character(len=200) :: pathnode = '/scratch/users/$USER/' !change if needed
   character(len=200) :: submitdir
   integer :: ntasks
   integer :: slurmversion  ! major*100+minor, e.g. 2505 for 25.05.x; 0 if unknown
   integer :: nnodes = 1
   ! per node: Slurm node name (blank for single node), CPUs (as counted by
   ! Slurm) requested per srun step, MB per CPU for srun steps (0 if unknown)
   character(len=64), allocatable :: node_name(:)
   integer, allocatable :: node_cpus_per_slot(:), node_mem_per_cpu(:)
   ! slots (physical cores, or GPU job slots) per node
   integer, allocatable :: node_nslots(:)
   ! node index of each slot, and GPU index (job-relative, 0-based; -1 in CPU mode)
   integer, allocatable :: slot_node(:), slot_gpu(:)
   ! per-node slot index of each slot (0-based; selects the physical core when the
   ! job pins itself), and all job CPUs per node (requested by shared-GPU steps)
   integer, allocatable :: slot_local(:), node_cpus_total(:)
   ! GPU mode: jobs sharing one GPU, MPS used for sharing, GPUs per node
   integer :: jobs_per_gpu = 1
   logical :: use_mps = .false.
   integer :: ngpu_node = 0
   logical :: runlocal = .false.

   interface
      !> C helper counting hardware threads and physical cores in the affinity mask
      function get_cpu_topology(nthreads, ncores) bind(C, name="get_cpu_topology")
         use iso_c_binding, only: c_int
         integer(c_int) :: get_cpu_topology
         integer(c_int), intent(out) :: nthreads, ncores
      end function get_cpu_topology
   end interface
   
   contains

      !> Sets up the slot table: one slot per physical core, and in GPU mode
      !> at most jobs_per_gpu slots per GPU on each node.
      subroutine get_ntasks(gput)
         implicit none
         logical, intent(in) :: gput

         if (env_int("SLURM_JOB_NUM_NODES") > 1) then
            call get_slots_multinode()
         else
            call get_ncores()
         end if
         if (gput) call get_gpu_slots()
      end subroutine get_ntasks

      !> GPU mode: limits the slots per node to ngpu_node*jobs_per_gpu and
      !> assigns each slot a GPU index (job-relative numbering, starting at 0).
      !> All nodes are assumed to have the same number of GPUs in the job.
      subroutine get_gpu_slots()
         use logger
         implicit none
         character(len=250) :: str
         character(len=200) :: msg
         integer :: i, stat, gpn

         ngpu_node = env_int("SLURM_GPUS_ON_NODE")
         if (ngpu_node <= 0) then
            ! fall back to the GPUs visible to this process
            call get_environment_variable("CUDA_VISIBLE_DEVICES", value=str, status=stat)
            if (stat == 0 .and. len_trim(str) > 0) then
               ngpu_node = 1
               do i = 1, len_trim(str)
                  if (str(i:i) == ',') ngpu_node = ngpu_node + 1
               end do
            end if
         end if
         if (ngpu_node <= 0) then
            call log_message(3, "No GPUs found (SLURM_GPUS_ON_NODE, CUDA_VISIBLE_DEVICES), cannot run in GPU mode")
            stop 1
         end if
         if (nnodes > 1) then
            gpn = env_int("SLURM_GPUS_PER_NODE")
            if (gpn > 0 .and. gpn /= ngpu_node) then
               call log_message(3, "SLURM_GPUS_PER_NODE differs from SLURM_GPUS_ON_NODE")
               stop 1
            else if (gpn <= 0) then
               call log_message(2, "Assuming the same number of GPUs on every node as on this one")
            end if
         end if

         do i = 1, nnodes
            if (node_nslots(i) < ngpu_node*jobs_per_gpu) then
               write(msg,'(a,i6,a,i6,a)') "Only ", node_nslots(i), " physical cores for ", &
                  ngpu_node*jobs_per_gpu, " GPU slots on node "//trim(node_name(i))
               call log_message(2, msg)
            end if
            node_nslots(i) = min(node_nslots(i), ngpu_node*jobs_per_gpu)
         end do
         call build_slot_table()

         write(msg,'(a,i4,a,i4,a,l1,a,i6)') "GPU mode: GPUs per node=", ngpu_node, &
            " jobs per GPU=", jobs_per_gpu, " MPS=", use_mps, " Slots=", ntasks
         call log_message(1, msg)
      end subroutine get_gpu_slots

      !> Fills slot_node and slot_gpu from node_nslots, round-robin over nodes
      !> so that jobs spread evenly over nodes (and GPUs) when njobs < ntasks.
      subroutine build_slot_table()
         implicit none
         integer :: c, j, k

         if (allocated(slot_node)) deallocate(slot_node)
         if (allocated(slot_gpu)) deallocate(slot_gpu)
         if (allocated(slot_local)) deallocate(slot_local)
         ntasks = sum(node_nslots)
         allocate(slot_node(ntasks), slot_gpu(ntasks), slot_local(ntasks))
         k = 0
         do c = 1, maxval(node_nslots)
            do j = 1, nnodes
               if (node_nslots(j) >= c) then
                  k = k + 1
                  slot_node(k) = j
                  slot_local(k) = c - 1
                  if (ngpu_node > 0) then
                     slot_gpu(k) = mod(c-1, ngpu_node)
                  else
                     slot_gpu(k) = -1
                  end if
               end if
            end do
         end do
      end subroutine build_slot_table

      !> Sets ntasks to the number of physical cores available to the job, so
      !> that one job runs per core irrespective of hyperthreading, and sets
      !> cpus_per_slot/mem_per_cpu so that each srun step requests one core.
      subroutine get_ncores()
         use logger
         use iso_c_binding, only: c_int
         implicit none
         integer(c_int) :: nthreads, ncores
         integer :: slurm_ntasks, cpus_on_node, mem_per_node, tpc
         integer :: cpus_per_slot, mem_per_cpu
         character(len=200) :: msg

         if (get_cpu_topology(nthreads, ncores) /= 0 .or. ncores < 1) then
            call log_message(3, "Could not determine CPU topology from affinity mask")
            stop 1
         end if
         tpc = max(1, nthreads/ncores)

         slurm_ntasks = env_int("SLURM_NTASKS")
         cpus_on_node = env_int("SLURM_CPUS_ON_NODE")

         write(msg,'(a,i6,a,i6,a,i6,a,i6)') "Affinity mask: hw threads=", nthreads, &
            " physical cores=", ncores, " SLURM_NTASKS=", slurm_ntasks, &
            " SLURM_CPUS_ON_NODE=", cpus_on_node
         call log_message(1, msg)

         ntasks = ncores
         if (cpus_on_node > 0) then
            if (nthreads > cpus_on_node .and. tpc > 1) then
               ! mask is larger than the allocation (cores not constrained by cgroup)
               ntasks = max(1, cpus_on_node/tpc)
               cpus_per_slot = tpc
               call log_message(2, "Affinity mask larger than SLURM_CPUS_ON_NODE, "// &
                  "using SLURM_CPUS_ON_NODE/threads_per_core")
            else
               ! CPUs as Slurm counts them per physical core in this allocation
               cpus_per_slot = max(1, cpus_on_node/ncores)
            end if
         else
            cpus_per_slot = tpc
            call log_message(2, "SLURM_CPUS_ON_NODE not set, using threads per core for step size")
         end if
         if (mod(nthreads, ncores) /= 0) then
            call log_message(2, "Uneven threads per core (hybrid CPU?), step size may be inexact")
         end if
         if (slurm_ntasks > 0 .and. slurm_ntasks /= ntasks) then
            write(msg,'(a,i6,a,i6,a)') "SLURM_NTASKS=", slurm_ntasks, " but ", ntasks, &
               " physical cores available: running one job per physical core"
            call log_message(2, msg)
         end if

         ! memory per CPU for the steps, so that steps do not claim all job memory
         mem_per_cpu = env_int("SLURM_MEM_PER_CPU")
         if (mem_per_cpu <= 0) then
            mem_per_node = env_int("SLURM_MEM_PER_NODE")
            if (mem_per_node > 0 .and. cpus_on_node > 0) then
               mem_per_cpu = mem_per_node/cpus_on_node
            else
               mem_per_cpu = 0
               call log_message(2, "No SLURM_MEM_PER_CPU/SLURM_MEM_PER_NODE, steps request no memory")
            end if
         end if

         write(msg,'(a,i6,a,i6,a,i8)') "Slots=", ntasks, " CPUs per step=", cpus_per_slot, &
            " MB per CPU=", mem_per_cpu
         call log_message(1, msg)

         nnodes = 1
         allocate(node_name(1), node_cpus_per_slot(1), node_mem_per_cpu(1), node_nslots(1))
         allocate(node_cpus_total(1))
         node_name(1) = ""
         node_cpus_per_slot(1) = cpus_per_slot
         node_mem_per_cpu(1) = mem_per_cpu
         node_nslots(1) = ntasks
         if (cpus_on_node > 0) then
            node_cpus_total(1) = cpus_on_node
         else
            node_cpus_total(1) = nthreads
         end if
         call build_slot_table()
      end subroutine get_ncores

      !> Builds the slot table for an allocation spanning several nodes: one
      !> slot per physical core on each node. Node names from
      !> SLURM_JOB_NODELIST, CPUs per node from SLURM_JOB_CPUS_PER_NODE and
      !> ThreadsPerCore per node from scontrol; cores = CPUs/ThreadsPerCore.
      subroutine get_slots_multinode()
         use logger
         use iso_c_binding, only: c_int
         implicit none
         character(len=*), parameter :: fhosts = ".pt_hostnames.tmp", fnodes = ".pt_nodes.tmp"
         integer, allocatable :: node_cpus(:), node_tpc(:), node_cores(:)
         integer(c_int) :: nthreads, ncores
         character(len=250) :: str
         character(len=64) :: name, localnode
         character(len=200) :: msg
         integer :: i, t, nread, stat, mem_cpu_job, mem_per_node
         integer :: unit = 502

         nnodes = env_int("SLURM_JOB_NUM_NODES")
         allocate(node_name(nnodes), node_cpus(nnodes), node_tpc(nnodes), node_cores(nnodes))
         allocate(node_cpus_per_slot(nnodes), node_mem_per_cpu(nnodes))

         ! (1) node names, in the same order as SLURM_JOB_CPUS_PER_NODE
         if (.not. run_to_file('scontrol show hostnames "$SLURM_JOB_NODELIST"', fhosts)) then
            call log_message(3, "scontrol show hostnames failed, cannot resolve node list")
            stop 1
         end if
         open(unit, file=fhosts, status="old", iostat=stat)
         nread = 0
         do i = 1, nnodes
            read(unit, '(a)', iostat=stat) node_name(i)
            if (stat /= 0) exit
            nread = nread + 1
         end do
         close(unit, status='delete')
         if (nread /= nnodes) then
            write(msg,'(a,i6,a,i6)') "Node list has ", nread, " entries, SLURM_JOB_NUM_NODES=", nnodes
            call log_message(3, msg)
            stop 1
         end if

         ! (2) CPUs per node
         call get_environment_variable("SLURM_JOB_CPUS_PER_NODE", value=str, status=stat)
         if (stat /= 0) then
            call log_message(3, "SLURM_JOB_CPUS_PER_NODE not set")
            stop 1
         end if
         call parse_cpus_per_node(str, nnodes, node_cpus, nread)
         if (nread /= nnodes .or. any(node_cpus < 1)) then
            call log_message(3, "Cannot parse SLURM_JOB_CPUS_PER_NODE="//trim(str))
            stop 1
         end if

         ! (3) threads per core per node, from lines "<NodeName> <ThreadsPerCore>"
         node_tpc(:) = 0
         if (.not. run_to_file('scontrol show node -o "$SLURM_JOB_NODELIST" | awk ''{n="";t="";'// &
               'for(i=1;i<=NF;i++){split($i,a,"=");if(a[1]=="NodeName")n=a[2];'// &
               'if(a[1]=="ThreadsPerCore")t=a[2]}print n,t}''', fnodes)) then
            call log_message(3, "scontrol show node failed, cannot get ThreadsPerCore")
            stop 1
         end if
         open(unit, file=fnodes, status="old", iostat=stat)
         do
            read(unit, '(a)', iostat=stat) str
            if (stat /= 0) exit
            read(str, *, iostat=stat) name, t
            if (stat /= 0) cycle
            do i = 1, nnodes
               if (trim(node_name(i)) == trim(name)) node_tpc(i) = t
            end do
         end do
         close(unit, status='delete')
         do i = 1, nnodes
            if (node_tpc(i) < 1) then
               call log_message(3, "No ThreadsPerCore for node "//trim(node_name(i)))
               stop 1
            end if
         end do

         ! (4) cores, step size and memory per node
         mem_cpu_job = env_int("SLURM_MEM_PER_CPU")
         mem_per_node = env_int("SLURM_MEM_PER_NODE")
         if (mem_cpu_job <= 0 .and. mem_per_node <= 0) then
            call log_message(2, "No SLURM_MEM_PER_CPU/SLURM_MEM_PER_NODE, steps request no memory")
         end if
         do i = 1, nnodes
            node_cores(i) = max(1, node_cpus(i)/node_tpc(i))
            if (mod(node_cpus(i), node_tpc(i)) /= 0) then
               call log_message(2, "CPUs not a multiple of ThreadsPerCore on node "//trim(node_name(i)))
            end if
            node_cpus_per_slot(i) = node_tpc(i)
            if (mem_cpu_job > 0) then
               node_mem_per_cpu(i) = mem_cpu_job
            else if (mem_per_node > 0) then
               node_mem_per_cpu(i) = mem_per_node/node_cpus(i)
            else
               node_mem_per_cpu(i) = 0
            end if
         end do

         ! (5) cross-check this node against its affinity mask
         call get_environment_variable("SLURMD_NODENAME", value=localnode, status=stat)
         if (stat == 0 .and. get_cpu_topology(nthreads, ncores) == 0) then
            do i = 1, nnodes
               if (trim(node_name(i)) == trim(localnode) .and. ncores /= node_cores(i)) then
                  write(msg,'(a,i6,a,i6,a)') "Node "//trim(localnode)//": affinity mask has ", ncores, &
                     " physical cores, Slurm allocation gives ", node_cores(i), ", using Slurm value"
                  call log_message(2, msg)
               end if
            end do
         end if

         ! (6) slot table, round-robin over nodes so that jobs spread evenly
         allocate(node_nslots(nnodes), node_cpus_total(nnodes))
         node_nslots(:) = node_cores(:)
         node_cpus_total(:) = node_cpus(:)
         call build_slot_table()

         write(msg,'(a,i6,a,i6)') "Nodes=", nnodes, " Slots=", ntasks
         call log_message(1, msg)
         do i = 1, nnodes
            write(msg,'(a,i6,a,i4,a,i6,a,i4,a,i8)') "Node "//trim(node_name(i))//": CPUs=", node_cpus(i), &
               " ThreadsPerCore=", node_tpc(i), " slots=", node_cores(i), &
               " CPUs per step=", node_cpus_per_slot(i), " MB per CPU=", node_mem_per_cpu(i)
            call log_message(1, msg)
         end do
         deallocate(node_cpus, node_tpc, node_cores)
      end subroutine get_slots_multinode

      !> Expands a SLURM_JOB_CPUS_PER_NODE string, e.g. "16(x2),8" -> 16,16,8.
      !> nfound is the number of nodes described by str (may exceed n), -1 on error.
      subroutine parse_cpus_per_node(str, n, cpus, nfound)
         implicit none
         character(len=*), intent(in) :: str
         integer, intent(in) :: n
         integer, intent(out) :: cpus(n), nfound
         character(len=50) :: tok
         integer :: pos, last, ie, ip, iq, val, nrep, r, stat

         cpus(:) = 0
         nfound = 0
         pos = 1
         last = len_trim(str)
         do while (pos <= last)
            ie = index(str(pos:last), ',')
            if (ie == 0) then
               tok = str(pos:last)
               pos = last + 1
            else
               tok = str(pos:pos+ie-2)
               pos = pos + ie
            end if
            ip = index(tok, '(x')
            if (ip > 0) then
               iq = index(tok, ')')
               read(tok(1:ip-1), *, iostat=stat) val
               if (stat == 0) read(tok(ip+2:iq-1), *, iostat=stat) nrep
            else
               read(tok, *, iostat=stat) val
               nrep = 1
            end if
            if (stat /= 0) then
               nfound = -1
               return
            end if
            do r = 1, nrep
               nfound = nfound + 1
               if (nfound <= n) cpus(nfound) = val
            end do
         end do
      end subroutine parse_cpus_per_node

      !> Runs "cmd > fname" in a shell; true if the command succeeded.
      logical function run_to_file(cmd, fname)
         implicit none
         character(len=*), intent(in) :: cmd, fname
         integer :: cstat, estat
         call execute_command_line(cmd//" > "//fname, wait=.true., exitstat=estat, cmdstat=cstat)
         run_to_file = (cstat == 0 .and. estat == 0)
      end function run_to_file

      !> Warns if the submit directory is on a node-local filesystem, since job
      !> scripts and job directories must be visible from all nodes.
      subroutine check_shared_fs()
         use logger
         implicit none
         character(len=*), parameter :: ftype = ".pt_fstype.tmp"
         character(len=50) :: fstype
         integer :: stat
         integer :: unit = 503

         if (nnodes <= 1) return
         if (.not. run_to_file('stat -f -c %T "'//trim(submitdir)//'"', ftype)) then
            call log_message(2, "Could not determine filesystem type of "//trim(submitdir))
            return
         end if
         open(unit, file=ftype, status="old", iostat=stat)
         read(unit, '(a)', iostat=stat) fstype
         close(unit, status='delete')
         if (stat /= 0) return
         call log_message(1, "Submit directory filesystem type: "//trim(fstype))
         select case (trim(fstype))
         case ("ext2/ext3", "xfs", "tmpfs", "btrfs", "overlayfs", "ramfs")
            call log_message(2, "Submit directory looks node-local ("//trim(fstype)// &
               "), jobs on other nodes may not find their scripts")
         end select
      end subroutine check_shared_fs

      !> Returns an integer environment variable, or 0 if unset or unreadable.
      !> Values like "16(x2)" (SLURM_JOB_CPUS_PER_NODE style) are not handled.
      integer function env_int(name)
         implicit none
         character(len=*), intent(in) :: name
         character(len=250) :: str
         integer :: stat
         env_int = 0
         call get_environment_variable(name, value=str, status=stat)
         if (stat /= 0) return
         read(str, *, iostat=stat) env_int
         if (stat /= 0) env_int = 0
      end function env_int
      
      subroutine get_hostname()
         use logger
         implicit none
         integer :: stat
         integer, parameter :: hostlen = 100
         character(len=hostlen) :: hostname_buffer

         call get_environment_variable("HOSTNAME", value=hostname_buffer, status=stat)
         if (stat /= 0) then
            hostname = "unknown"
            call log_message(2, "Could not resolve hostname from environment variables")
         else
            hostname = trim(hostname_buffer)
         end if
      end subroutine get_hostname
      
      subroutine get_slurmversion()
         use logger
         implicit none
         integer :: slurmunit = 501
         integer :: stat
         
         integer :: major, minor

         ! "slurm 25.05.9" or "slurm-wlm 21.08.5" -> "25 05 9"
         call execute_command_line("sinfo --version | awk '{print $2}' | tr '.' ' ' > slurmversion", &
            wait=.true.,cmdstat=stat)
         if (stat /= 0) then
            slurmversion = 0
            call log_message(2, "Could not obtain SLURM version, setting it to 0")
            return
         end if

         open(unit=slurmunit, file="slurmversion", status="old",iostat=stat)
         if (stat /= 0) then
            slurmversion = 0
            call log_message(2, "Could not open SLURM version file, setting version to 0")
            return
         end if
         read(slurmunit,*,iostat=stat) major, minor
         close(slurmunit, status='delete')
         if (stat /= 0) then
            slurmversion = 0
            call log_message(2, "Could not parse SLURM version, setting version to 0")
            return
         end if
         slurmversion = 100*major + minor
      end subroutine get_slurmversion

      subroutine get_subdir()
         use logger
         implicit none
         integer :: stat
         call get_environment_variable("SLURM_SUBMIT_DIR", value=submitdir, status=stat)
         if (stat.ne.0) then
            call log_message(3, "Cannot resolve SLURM_SUBMIT_DIR")
            stop 1
         end if
      end subroutine get_subdir
end module HPCenvironment
