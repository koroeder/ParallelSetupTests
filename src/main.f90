program ParallelSetupTest
   use HPCenvironment
   use create_jobs
   implicit none

   integer :: ncores ! number of cores
   integer :: njobs ! number of jobs to be run
   logical :: gput ! using GPUS?
   character(len=10) :: inpdummy
   integer :: nargs
   integer :: jobcounter, j
   
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
   
   write(*,*) "Number of tasks: ", ntasks
   write(*,*) "Hostname: ", hostname
   write(*,*) "Slurm version: ", slurmversion
   
   !run jobs now
   jobcounter = 0 
   if (njobs.gt.ntasks) then
      do j=1,ntasks
         jobcounter = jobcounter + 1
      	 call job_string(jobcounter,"submit.sh")
      end do
   else
      do j=1,njobs
         jobcounter = jobcounter + 1
      	 call job_string(jobcounter,"submit.sh")
      end do
   end if
   
   do while (jobcounter.lt.njobs)
      jobcounter = jobcounter + 1
      call job_string(jobcounter,"submit.sh")
   end do
 
end program ParallelSetupTest
