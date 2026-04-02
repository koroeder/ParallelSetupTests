module logger
    implicit none
    integer, parameter :: LOG_LEVEL_DEBUG = 0
    integer, parameter :: LOG_LEVEL_INFO = 1
    integer, parameter :: LOG_LEVEL_WARNING = 2
    integer, parameter :: LOG_LEVEL_ERROR = 3
    integer :: log_level = LOG_LEVEL_INFO
 
 contains
    subroutine log_message(level, message)
       integer, intent(in) :: level
       character(len=*), intent(in) :: message
       if (level >= log_level) then
          select case(level)
          case(LOG_LEVEL_DEBUG); write(*, *) "[DEBUG] ", trim(message)
          case(LOG_LEVEL_INFO); write(*, *) "[INFO] ", trim(message)
          case(LOG_LEVEL_WARNING); write(*, *) "[WARNING] ", trim(message)
          case(LOG_LEVEL_ERROR); write(*, *) "[ERROR] ", trim(message)
          end select
       end if
    end subroutine log_message
 end module logger