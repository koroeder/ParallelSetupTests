module create_jobs

   contains
      subroutine job_string(jobid,fname,slot,gput)
         use logger
         use HPCenvironment, only: submitdir, jobs_per_gpu, use_mps, slot_gpu, slot_local
         implicit none
         integer, intent(in) :: jobid, slot
         logical, intent(in) :: gput
         character(len=*), intent(in) :: fname
         character(len=10) :: jobstr, gstr, lstr
         character(len=150) :: target
         character(len=4000) :: fullstr
         character(len=2000) :: jobcmd
         character(len=:), allocatable :: tag
         real :: rand1, rand2
         integer :: time1, time2
         character(len=10) :: tstr1, tstr2
         integer :: sunit = 501
         integer :: stat
         
         !create some random numbers
         call random_seed()
         call random_number(rand1)
         call random_number(rand2)
         time1 = int(100.0*rand1)
         time2 = int(100.0*rand2)

         write(tstr1,'(i8)') time1
         write(tstr2,'(i8)') time2
         write(jobstr,'(i8)') jobid
         
         write(*,*) "Job string ", jobstr, "waiting times: ", tstr1, tstr2
         
         target = trim(adjustl(submitdir))//"/"//trim(adjustl(jobstr))
         !(0) diagnostics: node, step and CPUs this job may run on
         ! (mount namespace id: differs between steps if a step has a private /tmp)
         fullstr = 'echo "job '//trim(adjustl(jobstr))//' host=$(hostname) step=$SLURM_STEP_ID '// &
            'mntns=$(readlink /proc/self/ns/mnt) $(grep Cpus_allowed_list /proc/self/status)"'
         !(0a) shared GPUs: the step sees all job CPUs of the node (--overlap), so pin
         ! this job to the physical core of its slot first (the diagnostics line then
         ! shows the pinned CPUs)
         if (gput .and. jobs_per_gpu > 1) then
            write(lstr,'(i8)') slot_local(slot)
            fullstr = 'pin=$(./pin_core.sh '//trim(adjustl(lstr))//') && taskset -pc $pin $$ > /dev/null'// &
               ' || { echo "job '//trim(adjustl(jobstr))//' ERROR: cannot pin to core '// &
               trim(adjustl(lstr))//'"; exit 6; } ; '//trim(fullstr)
         end if
         !(0b) shared GPUs: check that the step sees the job's GPUs, select the
         ! GPU of this slot (job-relative index) and, with MPS, check the daemon
         if (gput .and. jobs_per_gpu > 1) then
            tag = 'job '//trim(adjustl(jobstr))
            write(gstr,'(i8)') slot_gpu(slot)
            fullstr = trim(adjustl(fullstr))//' ; [ -n "$CUDA_VISIBLE_DEVICES" ] || { echo "'//tag// &
               ' ERROR: no GPU visible in step"; exit 3; }'
            fullstr = trim(adjustl(fullstr))//' ; echo "'//tag//' step GPUs=$CUDA_VISIBLE_DEVICES using GPU '// &
               trim(adjustl(gstr))//'"'
            fullstr = trim(adjustl(fullstr))//' ; export CUDA_VISIBLE_DEVICES='//trim(adjustl(gstr))
            if (use_mps) then
               ! /dev/shm, not /tmp: /tmp is private per step on this cluster (AUDIT §17)
               fullstr = trim(adjustl(fullstr))//' ; export CUDA_MPS_PIPE_DIRECTORY='// &
                  '${PARALLELTEST_MPS_BASE:-/dev/shm}/mps_${SLURM_JOB_ID:-local}/pipe'
               ! on failure, report the client's message and what the step sees in the pipe directory
               fullstr = trim(adjustl(fullstr))//' ; mpsout=$(echo get_server_list | nvidia-cuda-mps-control 2>&1)'// &
                  ' || { echo "'//tag//' ERROR: MPS daemon not reachable: $mpsout"'// &
                  ' ; ls -la $CUDA_MPS_PIPE_DIRECTORY 2>&1 ; exit 4; }'
            end if
         end if
         !(1) create new directory
         fullstr = trim(adjustl(fullstr))//" ; mkdir -p "//trim(adjustl(target))
         !(2) enter directory
         fullstr = trim(adjustl(fullstr))//" ; cd "//trim(adjustl(target))
         !(3) now wait 
         fullstr = trim(adjustl(fullstr))//" ; sleep "//trim(adjustl(tstr1))
         !(4) create an output
         fullstr = trim(adjustl(fullstr))//" ; echo 'hello world' > here"
         !(4b) optional real workload for testing, e.g. a pmemd.cuda command line
         call get_environment_variable("PARALLELTEST_JOB_CMD", value=jobcmd, status=stat)
         if (stat == 0 .and. len_trim(jobcmd) > 0) then
            fullstr = trim(adjustl(fullstr))//" ; "//trim(adjustl(jobcmd))// &
               ' || { echo "job '//trim(adjustl(jobstr))//' ERROR: job command failed"; exit 5; }'
         end if
         !(5) now wait again
         fullstr = trim(adjustl(fullstr))//" ; sleep "//trim(adjustl(tstr2))
         
         !create submission script
         open(sunit,file=trim(adjustl(fname)),status='unknown', iostat=stat)
         if (stat /= 0) then
            call log_message(3, "Failed to create submission script "//trim(fname))
            stop 1
         end if
         write(sunit,'(a)') '#!/bin/bash'
         write(sunit,'(a)') trim(adjustl(fullstr))
         close(sunit)
         
         !make script executable
         call execute_command_line("chmod +x "//trim(adjustl(fname)),wait=.true.,cmdstat=stat)
         if (stat /= 0) then
            call log_message(3, "Failed to make script executable")
            stop 1
         end if
      end subroutine job_string


end module create_jobs
