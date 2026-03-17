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
   
   ! check number of arguments
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
   
   allocate(pid(ntasks))

   write(*,*) "Number of tasks: ", ntasks
   write(*,*) "Hostname: ", hostname
   write(*,*) "Slurm version: ", slurmversion
   write(*,*) "Submission directory: ", submitdir
   !run jobs now
   jobcounter = 0 
   if (njobs.gt.ntasks) then
      do j=1,ntasks
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname)
         call fork_subproc(pid(j))
         if (pid(j).eq.0) then
            call submit_new_proc(fname,gput)
         end if
      end do
   else
      do j=1,njobs
         jobcounter = jobcounter + 1
         write(jobstr,'(i8)') jobcounter
         fname = "submit"//trim(adjustl(jobstr))//".sh"
         call job_string(jobcounter,fname)
         call fork_subproc(pid(j))
         if (pid(j).eq.0) then
            call submit_new_proc(fname,gput)
         end if
      end do
   end if

   
   do while (jobcounter.lt.njobs)
      killed=.false.
      call wait_subproc(piddone,status)
      newjob = -1
      if (piddone.gt.0) then
         write(*,*) "PID ", piddone, " finished with exit status: ", status
         do j=1,ntasks
            if (piddone.eq.pid(j)) then
               if (status.ne.0) killed=.true.
               newjob = j
               write(*,*) "PID ", piddone, " has finished on core ", j
            end if
         end do
      else
         write(*,*) " WARNING> wait returned a system error code", -PIDDONE
         call execute_command_line("sleep 1.0")
         call wait_subproc(piddone,status)
         write(*,*) " On calling wait again pid=", piddone ,' status=', status
         if (piddone.le.0) write(*,*) "WARNING> continuing for non-positive process id"
         if (piddone.lt.0) stop
      end if

      jobcounter = jobcounter + 1
      write(jobstr,'(i8)') jobcounter
      fname = "submit"//trim(adjustl(jobstr))//".sh"
      call job_string(jobcounter,fname)
      call fork_subproc(pid(newjob))
      if (pid(newjob).eq.0) then
         call submit_new_proc(fname,gput)
      end if
   end do
 
   !need to wait for the remaining jobs
   do j=1,ntasks
      call wait_subproc(piddone,status)
      if (piddone.gt.0) then
         do k=1,ntasks
            if (piddone.eq.pid(k)) then
               write(*,*) "PID ", piddone, " has finished on core ", k
            end if
         end do
      !note that we have not the same catching mechanism as above - this is normally needed, but we ignore it for this test setup
      end if
   end do

   write(*,*) " >>> Finished all jobs <<< "
end program ParallelSetupTest
