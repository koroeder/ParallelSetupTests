program ParallelSetupTest
   use HPCenvironment
   use create_jobs
   use utils
   implicit none

   integer :: njobs ! number of jobs to be run
   logical :: gput ! using GPUS?
   character(len=10) :: inpdummy
   integer :: nargs
   integer :: jobcounter, j, k
   character(len=10) :: jobstr
   integer, allocatable :: pid(:)
   character(len=100) :: fname
   integer :: piddone, status, newjob
   logical :: killed
   integer :: nfree  ! Number of free slots
   integer, allocatable :: free_slots(:)
   logical :: nomps, found
   integer :: iarg, ival, stat

   ! Check number of arguments
   nargs = command_argument_count()
   
   ! first argument is the number of jobs
   call get_command_argument(1,inpdummy)
   read(inpdummy,*) njobs
   
   gput = .false.
   nomps = .false.
   ! check for second argument
   if (nargs.gt.1) then
      call get_command_argument(2,inpdummy)
      if ((inpdummy.eq."gpu").or.(inpdummy.eq."GPU")) then
         gput = .true.
      end if
      ! further arguments: an integer = jobs per GPU, "nomps", "runlocal"
      do iarg = 3, nargs
         call get_command_argument(iarg,inpdummy)
         if (inpdummy.eq."runlocal") then
            runlocal = .true.
         else if (inpdummy.eq."nomps") then
            nomps = .true.
         else
            read(inpdummy,*,iostat=stat) ival
            if (stat == 0 .and. ival >= 1) then
               jobs_per_gpu = ival
            else
               write(*,*) "Unknown argument ", trim(inpdummy)
               stop 1
            end if
         end if
      end do
   end if
   ! several jobs per GPU share it via MPS unless "nomps" (plain time-slicing)
   use_mps = gput .and. jobs_per_gpu > 1 .and. .not. nomps

   !get environment variables
   call get_ntasks(gput)
   call get_hostname()
   call get_slurmversion()
   call get_subdir()
   call check_shared_fs()
   if (use_mps) call start_mps()

   allocate(pid(ntasks), free_slots(ntasks))

   write(*,*) "Number of tasks: ", ntasks
   write(*,*) "Hostname: ", hostname
   write(*,*) "Slurm version: ", slurmversion
   write(*,*) "Submission directory: ", submitdir

   ! Initialize free slots (all slots are initially free)
   nfree = ntasks
   do j = 1, ntasks
      free_slots(j) = ntasks + 1 - j  ! slot 1 is used first (popped from the end)
   end do

   !run initial jobs
   jobcounter = 0 
   if (njobs.gt.ntasks) then
      do j=1,ntasks
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname,free_slots(nfree),gput)
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname,gput,free_slots(nfree))
         end if
         nfree = nfree - 1
      end do
   else
      do j=1,njobs
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname,free_slots(nfree),gput)
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname,gput,free_slots(nfree))
         end if
         nfree = nfree - 1
      end do
   end if

   ! reuse free slots as children finish
   do while (jobcounter < njobs)
      killed = .false.
      call wait_subproc(piddone, status)
      if (piddone > 0) then
         write(*, *) "PID ", piddone, " finished with exit status: ", status
         call check_mps_pid(piddone)
         ! Find the slot corresponding to piddone
         found = .false.
         do j = 1, ntasks
            if (pid(j) == piddone) then
               if (status /= 0) killed = .true.
               write(*, *) "PID ", piddone, " has finished on core ", j
               ! Add this slot back to the free list
               nfree = nfree + 1
               free_slots(nfree) = j
               found = .true.
               exit
            end if
         end do
         if (.not. found) then
            write(*, *) "WARNING: PID ", piddone, " is not a job of this run"
            cycle
         end if
      else
         write(*, *) " WARNING> wait returned a system error code ", -piddone
         call execute_command_line("sleep 1.0")
         cycle
      end if

      ! assign a new job to the freed slot
      jobcounter = jobcounter + 1
      write(jobstr,'(i8)') jobcounter
      fname = "submit"//trim(adjustl(jobstr))//".sh"
      if (nfree > 0) then
         call job_string(jobcounter, fname, free_slots(nfree), gput)
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname, gput, free_slots(nfree))
         end if
         nfree = nfree - 1
      else
         write(*, *) "WARNING: No free slots available (should not happen)"
      end if
   end do

   ! Wait for all remaining children to finish
   do while (nfree < ntasks)
      call wait_subproc(piddone, status)
      if (piddone > 0) then
         call check_mps_pid(piddone)
         do j = 1, ntasks
            if (pid(j) == piddone) then
               write(*, *) "PID ", piddone, " has finished on core ", j
               nfree = nfree + 1
               free_slots(nfree) = j
               exit
            end if
         end do
      else
         write(*, *) "WARNING: wait returned error code ", -piddone
         call execute_command_line("sleep 1.0")
      end if
   end do

   if (use_mps) call stop_mps()
   write(*, *) " >>> Finished all jobs <<< "
   deallocate(pid, free_slots)

contains

   !> An MPS daemon step must run until stop_mps; if it ends early, the jobs
   !> on that node lose MPS, so stop and report instead of continuing.
   subroutine check_mps_pid(p)
      integer, intent(in) :: p
      if (.not. use_mps) return
      if (any(mps_pid == p)) then
         write(*, *) "ERROR: MPS daemon step (PID ", p, ") ended before all jobs finished"
         stop 1
      end if
   end subroutine check_mps_pid
end program ParallelSetupTest
