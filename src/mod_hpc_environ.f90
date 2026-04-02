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
         use logger
         implicit none
         logical, intent(in) :: gput
         character(len=250) :: str_ngpus, str_ntasks
         integer :: stat
         
         if (gput) then
            call get_environment_variable("SLURM_GPU", value=str_ngpus, status=stat)
            if (stat /= 0) then
               call log_message(3,  "SLURM_GPU not set, cannot get number of GPUS to be used")
               stop 1
            end if
            read(str_ngpus,'(i8)') ntasks
         else
            call get_environment_variable("SLURM_NTASKS", value=str_ntasks, status=stat)
            if (stat /= 0) then
               call log_message(3,  "SLURM_NTASKS not set, cannot get number of CPUS to be used")
               stop 1
            end if
            read(str_ntasks,'(i8)') ntasks
         end if
      end subroutine get_ntasks
      
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
         
         call execute_command_line("sinfo --version | sed 's/-wlm//' |cut -c7-8 > slurmversion",wait=.true.,cmdstat=stat)
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
         read(slurmunit,'(i8)') slurmversion
         close(slurmunit, status='delete')
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
