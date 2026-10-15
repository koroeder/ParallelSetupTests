module utils

   !utilities for forking and waiting
   interface
      !> C-binding to forking new job for gfortran
      function gfortran_fork() bind(C, name="fork")
         use iso_c_binding, only: c_int
            integer(c_int) :: gfortran_fork
      end function gfortran_fork

      !> C-binding for wait command in gfortran
      function gfortran_wait(ExitStatus) bind(C, name="wait")
         use iso_c_binding, only: c_int
         integer(c_int) :: gfortran_wait
         integer(c_int), intent(out) :: ExitStatus
      end function gfortran_wait

      !> C-binding for waitpid, used with WNOHANG to poll a single child
      function c_waitpid(pid, ExitStatus, options) bind(C, name="waitpid")
         use iso_c_binding, only: c_int
         integer(c_int) :: c_waitpid
         integer(c_int), value :: pid, options
         integer(c_int), intent(out) :: ExitStatus
      end function c_waitpid
   end interface

   !wrapper for execution
   interface
      function exec_wrapper(cmd) bind(C, name="exec_wrapper")
         use iso_c_binding, only: c_char, c_int
         implicit none
         character(kind=c_char) :: cmd(*)
         integer(kind=c_int) :: exec_wrapper
      end function exec_wrapper
   end interface

   ! process ids of the srun steps running the MPS control daemons (one per node)
   integer, allocatable :: mps_pid(:)

   contains

      subroutine submit_new_proc(fname,gput,slot)
         use HPCenvironment, only: node_cpus_per_slot, node_mem_per_cpu, node_name, &
            slot_node, nnodes, slurmversion, jobs_per_gpu, node_cpus_total
         implicit none
         character(len=100), intent(in) :: fname
         logical, intent(in) :: gput
         integer, intent(in) :: slot
         character(len=20) :: cstr, mstr
         character(len=250) :: envflags
         character(len=:), allocatable :: excl, memflag, nodeflag, stepflags
         integer :: inode, stat

         ! one physical core per step: -c is the number of Slurm CPUs per core
         ! on the node of this slot (2 with hyperthreading), steps must not overlap
         inode = slot_node(slot)
         write(cstr,'(i8)') node_cpus_per_slot(inode)
         ! --exact exists from 20.11 on, before that --exclusive had this meaning
         if (slurmversion > 0 .and. slurmversion < 2011) then
            excl = ' --exclusive'
         else
            excl = ' --exact'
         end if
         stepflags = excl//' --cpu-bind=cores'
         if (gput) then
            if (jobs_per_gpu == 1) then
               ! one job per GPU: Slurm assigns the GPU to the step
               stepflags = ' --gres=gpu:1'//stepflags
            else
               ! shared GPUs: a step without --gres is allocated all job GPUs
               ! exclusively, so such steps run one after another (AUDIT §17).
               ! --overlap lets them run concurrently, but Slurm would put them
               ! all on the same core: request all job CPUs of the node and let
               ! the job script pin itself to its slot's core (pin_core.sh).
               ! Flags can be overridden for testing.
               write(cstr,'(i8)') node_cpus_total(inode)
               stepflags = ' --overlap'
               call get_environment_variable("PARALLELTEST_GPU_STEP_FLAGS", value=envflags, status=stat)
               if (stat == 0 .and. len_trim(envflags) > 0) stepflags = ' '//trim(adjustl(envflags))
            end if
         end if
         if (gput .and. jobs_per_gpu > 1) then
            ! overlapping steps share the job's memory; a per-CPU request for all CPUs would ask for all of it
            memflag = ''
         else if (node_mem_per_cpu(inode) > 0) then
            write(mstr,'(i12)') node_mem_per_cpu(inode)
            memflag = ' --mem-per-cpu='//trim(adjustl(mstr))
         else
            memflag = ''
         end if
         ! multi-node: place the step on the node of this slot
         if (nnodes > 1) then
            nodeflag = ' -w '//trim(node_name(inode))
         else
            nodeflag = ''
         end if
         call exec_subr('srun -n1 -N1'//nodeflag//' -c'//trim(adjustl(cstr))//stepflags// &
            memflag//' --label --wait=0 '//TRIM(ADJUSTL(FNAME)))
      end subroutine submit_new_proc

      !> Directory (in the shared submit directory) used to signal MPS daemon
      !> readiness and shutdown between the master and the nodes.
      function mps_ctrl_dir() result(dir)
         use HPCenvironment, only: submitdir
         implicit none
         character(len=:), allocatable :: dir
         character(len=50) :: jobid
         integer :: stat
         call get_environment_variable("SLURM_JOB_ID", value=jobid, status=stat)
         if (stat /= 0) jobid = "local"
         dir = trim(adjustl(submitdir))//"/.mps_"//trim(adjustl(jobid))
      end function mps_ctrl_dir

      !> Starts one MPS control daemon per node, each in its own long-lived
      !> srun step (--overlap, so it does not use up CPUs of the job steps),
      !> and waits until all daemons report ready. Stops on failure.
      subroutine start_mps()
         use logger
         use HPCenvironment, only: nnodes, node_name
         use iso_c_binding, only: c_int
         implicit none
         character(len=*), parameter :: dname = "mps_daemon.sh"
         integer, parameter :: timeout = 120
         character(len=:), allocatable :: ctrl, nodeflag
         character(len=10) :: istr
         character(len=200) :: msg
         integer :: i, t, nready, stat
         integer(c_int) :: wst
         integer(c_int), parameter :: WNOHANG = 1
         integer :: sunit = 504
         logical :: ex

         ctrl = mps_ctrl_dir()
         call execute_command_line("rm -rf "//ctrl//" ; mkdir -p "//ctrl, wait=.true., cmdstat=stat)
         if (stat /= 0) then
            call log_message(3, "Cannot create MPS control directory "//ctrl)
            stop 1
         end if

         open(sunit, file=dname, status='replace', iostat=stat)
         if (stat /= 0) then
            call log_message(3, "Failed to create "//dname)
            stop 1
         end if
         write(sunit,'(a)') '#!/bin/bash'
         write(sunit,'(a)') '# MPS control daemon for one node; runs until the stop file appears'
         write(sunit,'(a)') 'I=$1; CTRL=$2'
         write(sunit,'(a)') '# /dev/shm, not /tmp: /tmp is private per step on this cluster (private-tmpdir)'
         write(sunit,'(a)') 'D=${PARALLELTEST_MPS_BASE:-/dev/shm}/mps_${SLURM_JOB_ID:-local}'
         write(sunit,'(a)') 'export CUDA_MPS_PIPE_DIRECTORY=$D/pipe CUDA_MPS_LOG_DIRECTORY=$D/log'
         write(sunit,'(a)') '# on any exit: keep the control log in the shared control dir, remove the node-local dir'
         write(sunit,'(a)') 'trap "cp $D/log/control.log $CTRL/control_$I.log 2>/dev/null; rm -rf $D" EXIT'
         write(sunit,'(a)') 'mkdir -p $D/pipe $D/log || exit 1'
         write(sunit,'(a)') 'echo "MPS node $I host=$(hostname) step=$SLURM_STEP_ID '// &
            'mntns=$(readlink /proc/self/ns/mnt) GPUs=$CUDA_VISIBLE_DEVICES"'
         write(sunit,'(a)') '[ -n "$CUDA_VISIBLE_DEVICES" ] || { echo "MPS node $I ERROR: no GPU visible"; exit 2; }'
         write(sunit,'(a)') 'nvidia-cuda-mps-control -d || { echo "MPS node $I ERROR: daemon start failed"; exit 3; }'
         write(sunit,'(a)') 'ok=0; for n in $(seq 1 20); do'
         write(sunit,'(a)') '  echo get_server_list | nvidia-cuda-mps-control > /dev/null 2>&1 && { ok=1; break; }; sleep 1'
         write(sunit,'(a)') 'done'
         write(sunit,'(a)') '[ $ok = 1 ] || { echo "MPS node $I ERROR: control daemon not reachable"; exit 4; }'
         write(sunit,'(a)') 'touch $CTRL/ready_$I'
         write(sunit,'(a)') 'while [ ! -e $CTRL/stop ]; do sleep 2; done'
         write(sunit,'(a)') 'echo quit | nvidia-cuda-mps-control'
         write(sunit,'(a)') 'sleep 2'
         write(sunit,'(a)') 'echo "MPS node $I stopped"'
         close(sunit)
         call execute_command_line("chmod +x "//dname, wait=.true., cmdstat=stat)

         allocate(mps_pid(nnodes))
         do i = 1, nnodes
            write(istr,'(i8)') i
            if (nnodes > 1) then
               nodeflag = ' -w '//trim(node_name(i))
            else
               nodeflag = ''
            end if
            call fork_subproc(mps_pid(i))
            if (mps_pid(i) == 0) then
               call exec_subr('srun -n1 -N1'//nodeflag//' -c1 --overlap --label --wait=0 ./'//dname// &
                  ' '//trim(adjustl(istr))//' '//ctrl)
            end if
         end do

         ! wait until every node has reported ready
         do t = 1, timeout
            nready = 0
            do i = 1, nnodes
               write(istr,'(i8)') i
               inquire(file=ctrl//"/ready_"//trim(adjustl(istr)), exist=ex)
               if (ex) nready = nready + 1
            end do
            if (nready == nnodes) exit
            ! a daemon step that has already ended will never become ready
            do i = 1, nnodes
               if (c_waitpid(int(mps_pid(i), c_int), wst, WNOHANG) == mps_pid(i)) then
                  write(istr,'(i8)') i
                  call log_message(3, "MPS daemon step for node "//trim(adjustl(istr))// &
                     " ended during start-up; see MPS lines in the output")
                  stop 1
               end if
            end do
            call execute_command_line("sleep 1", wait=.true.)
         end do
         if (nready /= nnodes) then
            write(msg,'(a,i4,a,i4,a,i4,a)') "MPS daemons ready on ", nready, " of ", nnodes, &
               " nodes after ", timeout, " s; see MPS lines in the output"
            call log_message(3, msg)
            stop 1
         end if
         call log_message(1, "MPS daemons running on all nodes")
      end subroutine start_mps

      !> Writes pin_core.sh: prints the CPU list (hyperthread siblings) of the
      !> K-th physical core (0-based) in the caller's affinity mask. Used by
      !> job scripts of shared-GPU jobs, whose steps see all job CPUs of a node.
      subroutine write_pin_script()
         use logger
         implicit none
         character(len=*), parameter :: pname = "pin_core.sh"
         integer :: stat
         integer :: sunit = 506

         open(sunit, file=pname, status='replace', iostat=stat)
         if (stat /= 0) then
            call log_message(3, "Failed to create "//pname)
            stop 1
         end if
         write(sunit,'(a)') '#!/bin/bash'
         write(sunit,'(a)') '# prints the CPUs of the K-th physical core (0-based) in this process''s affinity mask'
         write(sunit,'(a)') 'K=$1'
         write(sunit,'(a)') 'list=$(grep Cpus_allowed_list /proc/self/status | cut -f2)'
         write(sunit,'(a)') 'declare -A members; order=()'
         write(sunit,'(a)') 'IFS=, read -ra parts <<< "$list"'
         write(sunit,'(a)') 'for p in "${parts[@]}"; do'
         write(sunit,'(a)') '  if [[ $p == *-* ]]; then cpus=$(seq ${p%-*} ${p#*-}); else cpus=$p; fi'
         write(sunit,'(a)') '  for c in $cpus; do'
         write(sunit,'(a)') '    s=$(cat /sys/devices/system/cpu/cpu$c/topology/thread_siblings_list 2>/dev/null || echo $c)'
         write(sunit,'(a)') '    f=${s%%[,-]*}'
         write(sunit,'(a)') '    [ -z "${members[$f]}" ] && order+=($f)'
         write(sunit,'(a)') '    members[$f]="${members[$f]:+${members[$f]},}$c"'
         write(sunit,'(a)') '  done'
         write(sunit,'(a)') 'done'
         write(sunit,'(a)') '[ "$K" -lt "${#order[@]}" ] || { echo "core $K not in mask $list" >&2; exit 1; }'
         write(sunit,'(a)') 'echo "${members[${order[$K]}]}"'
         close(sunit)
         call execute_command_line("chmod +x "//pname, wait=.true., cmdstat=stat)
      end subroutine write_pin_script

      !> Signals the MPS daemons to quit and waits for their srun steps.
      subroutine stop_mps()
         use logger
         implicit none
         character(len=:), allocatable :: ctrl
         integer :: ndone, piddone, status, sunit = 505

         ctrl = mps_ctrl_dir()
         open(sunit, file=ctrl//"/stop", status='replace')
         close(sunit)
         ndone = 0
         do while (ndone < size(mps_pid))
            call wait_subproc(piddone, status)
            if (piddone < 0) exit
            if (any(mps_pid == piddone)) then
               ndone = ndone + 1
               if (status /= 0) call log_message(2, "An MPS daemon step ended with non-zero status")
            end if
         end do
         call log_message(1, "MPS daemons stopped; control logs in "//ctrl)
      end subroutine stop_mps

      subroutine wait_subproc(pid, ExitStatus)
         use logger
         use, intrinsic :: iso_c_binding
         implicit none
         integer, intent(out) :: pid, ExitStatus
         integer :: ret, wstatus
      
         ret = gfortran_wait(wstatus)
         if (ret < 0) then
            pid = -1
            ExitStatus = -1
            call log_message(3, "wait failed, returned negative process id")
         else
            pid = ret
            ! Extract exit status (portable way)
            if (WIFEXITED(wstatus)) then
               ExitStatus = WEXITSTATUS(wstatus)
            else
               ExitStatus = -1
            end if
         end if
      end subroutine wait_subproc

      subroutine getpid_subproc(pid)
         implicit none
         integer,intent(out) :: pid
         integer :: getpid
         pid=getpid()
      end subroutine getpid_subproc
      
      subroutine fork_subproc(pid) ! Parent: returns child PID; Child: returns 0
         use logger
         use, intrinsic :: iso_c_binding
         implicit none
         integer, intent(out) :: pid
   
         pid = gfortran_fork()
         if (pid < 0) then
            call log_message(3, "fork failed, returned negative process id")
            stop 1
         else if (pid == 0) then
            ! Child process: continue execution
            return
         end if
         ! Parent process: returns child PID
      end subroutine fork_subproc

      subroutine exec_subr(cmd)
         use logger
         use iso_c_binding, only: c_null_char
         implicit none
         character(len=*), intent(in) :: cmd
         integer :: stat
      
         ! Use exec_wrapper (C wrapper)
         stat = exec_wrapper(trim(adjustl(cmd))//c_null_char)
         if (stat.ne.0) then
            call log_message(3, "exec wrapper failed for command "//trim(cmd))
            stop 1
         end if
      end subroutine exec_subr

      function WIFEXITED(wstatus) result(res)
         integer, intent(in) :: wstatus
         logical :: res
         ! Platform-specific: Linux uses 8-bit shift for exit status
         res = (iand(wstatus, 127) == 0)
      end function WIFEXITED
   
      function WEXITSTATUS(wstatus) result(res)
         integer, intent(in) :: wstatus
         integer :: res
         res = iand(ishft(wstatus, -8), 255)
      end function WEXITSTATUS
end module utils