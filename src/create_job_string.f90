module create_jobs

   contains
      subroutine job_string(jobid,fname)
         use HPCenvironment, only: hostname
         implicit none
         integer, intent(in) :: jobid
         character(len=*), intent(in) :: fname
         character(len=10) :: jobstr
         character(len=150) :: target
         character(len=1000) :: fullstr
         real :: rand1, rand2
         integer :: time1, time2
         character(len=10) :: tstr1, tstr2
         integer :: sunit = 501
         
         !create some random numbers
         call random_seed()
         call random_number(rand1)
         call random_number(rand2)
         time1 = int(100.0*rand1)
         time2 = int(100.0*rand2)

         write(*,*) "rand1: ", rand1, " time1: ", time1
         write(*,*) "rand2: ", rand2, " time2: ", time2

         write(tstr1,'(i8)') time1
         write(tstr2,'(i8)') time2
         write(jobstr,'(i8)') jobid
         
         write(*,*) "Job string ", jobstr, "waiting times: ", tstr1, tstr2
         
         target = trim(adjustl(hostname))//':$SLURM_SUBMIT_DIR/'//trim(adjustl(jobstr))
         !(1) create new directory
         fullstr = "mkdir -p "//trim(adjustl(target))
         !(2) enter directory
         fullstr = trim(adjustl(fullstr))//" ; cd "//trim(adjustl(target))
         !(3) now wait 
         fullstr = trim(adjustl(fullstr))//" ; sleep("//trim(adjustl(tstr1))//")"
         !(4) create an output
         fullstr = trim(adjustl(fullstr))//" ; echo 'hello world' > here"
         !(5) now wait again
         fullstr = trim(adjustl(fullstr))//" ; sleep("//trim(adjustl(tstr2))//")"
         
         !create submission script
         open(sunit,file=trim(adjustl(fname)),status='unknown')
         write(sunit,*) '#!/bin/bash'
         write(sunit,*) fullstr
         close(sunit)
         
         !make script executable
         call execute_command_line("chmod +x "//trim(adjustl(fname)))
      end subroutine job_string


end module create_jobs
