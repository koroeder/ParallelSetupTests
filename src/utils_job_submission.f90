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

   contains

      subroutine submit_new_proc(fname,gput)
         implicit none
         character(len=100), intent(in) :: fname
         logical, intent(in) :: gput
         character(len=100) :: basestr

         if (gput) then
            call exec_subr('srun -n1 -N1 --exclusive -l --gres=gpu:1 ' // TRIM(ADJUSTL(FNAME)))
         else
            !call exec_subr('srun -n1 -N1 --exclusive -l '//TRIM(ADJUSTL(FNAME)))
            call exec_subr('srun --ntasks=1 --cpus-per-task=1 --exclusive --label --wait=0 '//TRIM(ADJUSTL(FNAME)))
         end if
      end subroutine submit_new_proc

      subroutine wait_subproc(pid,ExitStatus)
         implicit none
         integer,intent(inout) :: pid,ExitStatus
         pid=gfortran_wait(ExitStatus)
         ExitStatus=ishft(ExitStatus,-8)
      end subroutine wait_subproc

      subroutine getpid_subproc(pid)
         implicit none
         integer,intent(out) :: pid
         integer :: getpid
         pid=getpid()
      end subroutine getpid_subproc

      subroutine fork_subproc(pid) ! returns zero in the child process, PID of child in parent process
         implicit none
         integer, intent(inout) :: pid
         pid=gfortran_fork()
      end subroutine fork_subproc

      subroutine exec_subr(cmd)
         use iso_c_binding, only: c_null_char
         implicit none
         character(len=*), intent(in) :: cmd
         logical :: useexec = .true.
         integer :: stat
         
         if (useexec) then
            stat = exec_wrapper(trim(adjustl(cmd))//c_null_char)

            write(*,*) "execute> soemthing has gone wrong, iostat: ", stat
            stop
         else
            call execute_command_line(trim(adjustl(cmd)),wait=.true.,cmdstat=stat)
         end if
      end subroutine exec_subr
end module utils