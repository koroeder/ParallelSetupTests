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

   ! Check number of arguments
   nargs = command_argument_count()
   
   ! first argument is the number of jobs
   call get_command_argument(1,inpdummy)
   read(inpdummy,*) njobs
   
   gput = .false.
   ! check for second argument
   if (nargs.gt.1) then
      call get_command_argument(2,inpdummy)
      if ((inpdummy.eq."gpu").or.(inpdummy.eq."GPU")) then
         gput = .true.
      end if
      if (nargs.gt.2) then
         call get_command_argument(3,inpdummy)
         if (inpdummy.eq."runlocal") then
            runlocal = .true.
         end if
      end if
   end if

   !get environment variables
   call get_ntasks(gput)
   call get_hostname()
   call get_slurmversion()
   call get_subdir()

   allocate(pid(ntasks), free_slots(ntasks))

   write(*,*) "Number of tasks: ", ntasks
   write(*,*) "Hostname: ", hostname
   write(*,*) "Slurm version: ", slurmversion
   write(*,*) "Submission directory: ", submitdir

   ! Initialize free slots (all slots are initially free)
   nfree = ntasks
   do j = 1, ntasks
      free_slots(j) = j
   end do

   !run initial jobs
   jobcounter = 0 
   if (njobs.gt.ntasks) then
      do j=1,ntasks
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname)
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname,gput)
         end if
         nfree = nfree - 1
      end do
   else
      do j=1,njobs
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname)
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname,gput)
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
         ! Find the slot corresponding to piddone
         do j = 1, ntasks
            if (pid(j) == piddone) then
               if (status /= 0) killed = .true.
               write(*, *) "PID ", piddone, " has finished on core ", j
               ! Add this slot back to the free list
               nfree = nfree + 1
               free_slots(nfree) = j
               exit
            end if
         end do
      else
         write(*, *) " WARNING> wait returned a system error code ", -piddone
         call execute_command_line("sleep 1.0")
         cycle
      end if

      ! assign a new job to the freed slot
      jobcounter = jobcounter + 1
      write(jobstr,'(i8)') jobcounter
      fname = "submit"//trim(adjustl(jobstr))//".sh"
      call job_string(jobcounter, fname)
      if (nfree > 0) then
         call fork_subproc(pid(free_slots(nfree)))
         if (pid(free_slots(nfree)) == 0) then
            call submit_new_proc(fname, gput)
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

   write(*, *) " >>> Finished all jobs <<< "
   deallocate(pid, free_slots)
end program ParallelSetupTest
