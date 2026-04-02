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

      subroutine wait_subproc(pid, ExitStatus)
         use logger
         use, intrinsic :: iso_c_binding
         implicit none
         integer, intent(out) :: pid, ExitStatus
         integer :: ret, wstatus
      
         ret = gfortran_wait(wstatus)
         if (ret < 0) then
            pid = -1
            ExitStatus = -1
            call log_message(3, "wait failed, returned negative process id")
         else
            pid = ret
            ! Extract exit status (portable way)
            if (WIFEXITED(wstatus)) then
               ExitStatus = WEXITSTATUS(wstatus)
            else
               ExitStatus = -1
            end if
         end if
      end subroutine wait_subproc

      subroutine getpid_subproc(pid)
         implicit none
         integer,intent(out) :: pid
         integer :: getpid
         pid=getpid()
      end subroutine getpid_subproc
      
      subroutine fork_subproc(pid) ! Parent: returns child PID; Child: returns 0
         use logger
         use, intrinsic :: iso_c_binding
         implicit none
         integer, intent(out) :: pid
   
         pid = gfortran_fork()
         if (pid < 0) then
            call log_message(3, "fork failed, returned negative process id")
            stop 1
         else if (pid == 0) then
            ! Child process: continue execution
            return
         end if
         ! Parent process: returns child PID
      end subroutine fork_subproc

      subroutine exec_subr(cmd)
         use logger
         use iso_c_binding, only: c_null_char
         implicit none
         character(len=*), intent(in) :: cmd
         integer :: stat
      
         ! Use exec_wrapper (C wrapper)
         stat = exec_wrapper(trim(adjustl(cmd))//c_null_char)
         if (stat.ne.0) then
            call log_message(3, "exec wrapper failed for command "//trim(cmd))
            stop 1
         end if
      end subroutine exec_subr

      function WIFEXITED(wstatus) result(res)
         integer, intent(in) :: wstatus
         logical :: res
         ! Platform-specific: Linux uses 8-bit shift for exit status
         res = (iand(wstatus, 127) == 0)
      end function WIFEXITED
   
      function WEXITSTATUS(wstatus) result(res)
         integer, intent(in) :: wstatus
         integer :: res
         res = iand(ishft(wstatus, -8), 255)
      end function WEXITSTATUS
end module utils