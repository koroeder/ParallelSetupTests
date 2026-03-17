program ParallelSetupTest
   use HPCenvironment
   use create_jobs
   implicit none

   integer :: njobs ! number of jobs to be run
   logical :: gput ! using GPUS?
   character(len=10) :: inpdummy
   integer :: nargs
   integer :: jobcounter, j
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
   
   allocate(pid(ntasks))

   write(*,*) "Number of tasks: ", ntasks
   write(*,*) "Hostname: ", hostname
   write(*,*) "Slurm version: ", slurmversion
   
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

         end do
      jobcounter = jobcounter + 1
      write(jobstr,'(i8)') jobcounter
      call job_string(jobcounter,"submit"//trim(adjustl(jobstr))//".sh")
   end do
 


   IF (PIDDONE.GT.0) THEN
      IF (DEBUG) WRITE(MYUNIT,'(A,I8,A,I6)') ' parexec> PID ',PIDDONE,' has finished with exit status ',STATUS
      DO J2=1,NTASKS
         IF (PIDDONE.EQ.PID(J2)) THEN
            IF (STATUS.NE.0) KILLED=.TRUE.
            ! INCOMPLETE JOBS WOULD IDEALLY RETURN A NON-ZERO EXIT CODE 
            NEWJOB = J2
            IF (DEBUG) WRITE(MYUNIT, '(2(A,I8))') ' parexec> PID ',PIDDONE,' has finished on core ',J2
         ENDIF
      ENDDO
   ELSE
      CALL FLUSH(MYUNIT) ! the child process may duplicate output without this line!
      WRITE(MYUNIT,'(A,I20)') ' parexec> WARNING - WAIT returned system error code ',-PIDDONE
      ! Try calling wait again to see if this fixes things. 
      CALL EXECUTE_COMMAND_LINE('sleep 1.0')
      CALL WAIT_SUBR(PIDDONE,STATUS)
      WRITE(MYUNIT,'(2(A,I8))') ' parexec> on calling wait again pid=',PIDDONE,' status=',STATUS
      IF (PIDDONE.LE.0) WRITE(MYUNIT,'(2(A,I8))') ' parexec> WARNING *** continuing for non-positive process id'
      IF (STATUS.NE.0) STOP
   END IF
   CALL FLUSH(MYUNIT) ! the child process may duplicate output without this line!

   JOBCOUNTER = JOBCOUNTER + 1
   !wait for a short period to not run out of source ports
   CALL EXECUTE_COMMAND_LINE("sleep 1.0")
   !create submission script (note that at this stage all relevant folders and files exist locally)
   CALL CREATE_JOBSTR(CURRGEN,JOBCOUNTER,SUBFILE)
   !flush output before we fork child process
   CALL FLUSH(MYUNIT)
   !now create child process
   CALL FORK_SUBR(PID(NEWJOB))
   IF (PID(NEWJOB).EQ.0) THEN
      CALL SUBMITJOB(SUBFILE)
   END IF
ENDDO
!need to wait for the remaining jobs
!at this stage the previous loop should have all cores filled with the final jobs
!this means there should be NTASKS jobs left to be collected
DO J1=1,NTASKS
   CALL FLUSH(MYUNIT)
   CALL WAIT_SUBR(PIDDONE,STATUS)
   !IF (DEBUG) WRITE(MYUNIT,'(A,5I8)') ' parexec> PIDDONE,STATUS,PID=',PIDDONE,STATUS,PID(1:NTASKS)
   CALL FLUSH(MYUNIT) ! the child process may duplicate output without this line!
   IF (PIDDONE.GT.0) THEN
      DO J2=1,NTASKS
         IF (PIDDONE.EQ.PID(J2)) THEN
            IF (DEBUG) WRITE(MYUNIT, '(2(A,I8))') ' parexec> PID ',PIDDONE,' has finished on core ',J2
         ENDIF
      ENDDO
   ELSE
      CALL FLUSH(6) ! the child process may duplicate output without this line!
      WRITE(MYUNIT,'(A,I20)') ' parexec> WARNING - WAIT returned system error code ',-PIDDONE
      ! Try calling wait again to see if this fixes things. 
      CALL EXECUTE_COMMAND_LINE('sleep 1.0')
      CALL WAIT_SUBR(PIDDONE,STATUS)
      WRITE(MYUNIT,'(2(A,I8))') ' parexec> on calling wait again pid=',PIDDONE,' status=',STATUS
      IF (PIDDONE.LE.0) WRITE(MYUNIT,'(2(A,I8))') ' parexec> WARNING *** continuing for non-positive process id'
      IF (STATUS.NE.0) STOP
   END IF               
ENDDO
!remove all submission scripts from this generation
CALL EXECUTE_COMMAND_LINE('rm submit_GEN_*.sh')
END SUBROUTINE PARALLEL_MUT

end program ParallelSetupTest
