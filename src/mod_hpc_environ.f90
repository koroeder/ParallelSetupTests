module HPCenvironment
   implicit none
   character(len=100) :: hostname
   character(len=200) :: pathnode = '/scratch/users/$USER/' !change if needed
   character(len=200) :: submitdir
   integer :: ntasks
   integer :: slurmversion
   logical :: runlocal = .false.
   
   contains

      subroutine get_ntasks(gput)
         implicit none
         logical, intent(in) :: gput
         character(len=250) :: str_ngpus, str_ntasks
         
         if (gput) then
            call get_environment_variable("SLURM_GPU",str_ngpus)
            read(str_ngpus,'(i8)') ntasks
         else
            call get_environment_variable("SLURM_NTASKS", str_ntasks)
            write(*,*) str_ntasks
            read(str_ntasks,'(i8)') ntasks
         end if
      end subroutine get_ntasks
      
      subroutine get_hostname()
         implicit none
         integer :: hostunit = 500
         
         call execute_command_line("hostname > currentnode")
         open(unit=hostunit, file="currentnode",status="old")
         read(hostunit,'(a)') hostname
         close(hostunit)
         call execute_command_line("rm currentnode")
      end subroutine get_hostname
      
      subroutine get_slurmversion()
         implicit none
         integer :: slurmunit = 501
         
         call execute_command_line("sinfo --version | sed 's/-wlm//' |cut -c7-8 > slurmversion")
         open(unit=slurmunit, file="slurmversion", status="old")
         read(slurmunit,'(i8)') slurmversion
         close(slurmunit)
         call execute_command_line("rm slurmversion")
      end subroutine get_slurmversion

      subroutine get_subdir()
         implicit none
         call get_environment_variable("SLURM_SUBMIT_DIR/", submitdir)
      end subroutine get_subdir
end module HPCenvironment
