program fpm_shell
use :: M_journal,   only : journal
use :: M_history,   only : redo
use :: M_cli2,      only : set_args ,sget,rget,lget,iget, rgets,igets,sgets, unnamed
use :: M_io,        only : which, get_env
! extensions: chdir
implicit NONE
integer,parameter             :: dp=kind(0.0d0)         ! calculator returns double precision values
integer,parameter             :: iclen_calc=256
character(len=iclen_calc)     :: line0                  ! input line
character(len=iclen_calc)     :: linet                  ! input line trimmed of leading spaces
integer                       :: ii                     ! location of end of verb on input lines
integer                       :: iin=5                  ! I/O unit to read input from
integer                       :: ios                    ! status returned by last READ
character(len=4096)           :: message
character(len=:),allocatable  :: oldread
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)
   oldread=''
   call setup()
   call set_args(' -read " " -replay " "',help_text,version_text)  ! parse command line arguments
   if(sget('read').ne.' ')then                                     ! found an initial file to read
      iin=iin+10
      open(unit=iin,file=trim(sget('read')),iostat=ios)
      if(ios.ne.0)iin=iin-10                               ! if open failed fall back to previous I/O unit
      iin=iin+10
   endif
   call journal('O',sget('replay'))                        ! turn on journal file
   INFINITE: do                                            ! start loop to read input lines
      if(iin.eq.5)then                                     ! only show the prompt when reading from stdin
         write(*,'(a)',advance='no')'fpm>'                 ! write prompt showing nesting level in if/else/endif
      elseif(iin.lt.5)then
         call journal('sc','*fpm-shell* error: internal value for input unit is ',iin)
         stop
      endif
      if(iin.eq.5)then
         read(iin,'(a)',iostat=ios)line0                 ! read next line of input
         ios=0
      else
         read(iin,'(a)',iostat=ios)line0                   ! read next line of input
      endif
      if(ios.ne.0)then                                     ! process an I/O error
         if(iin.eq.5)exit INFINITE                         ! if reading from stdin exit
         close(iin,iostat=ios)                             ! if reading from an alternate input file close it
         iin=iin-10                                        ! drop back to previous input file
         cycle INFINITE
      else
         !call journal('T',trim(line0))
      endif
      call redo(line0) ! store into history if not "r".         ! if "r", edit and return a line from the history editor
      linet=adjustl(line0)                                      ! trim leading whitespace
      ii=index(linet,' ')                                       ! find end of first word
      if(ii.eq.0)ii=len(linet)                                  ! if input line is completely full first word is entire line
      if(linet(:1).eq.'#')then                                  ! ignore comment lines
         cycle INFINITE
      endif
      select case(linet(:ii))                                   ! handle all the other verbs
      case('.','quit')                                          ! COMMAND: quit program
         call journal('*fpm-shell*: that''s all folks ...')
         stop
      case(' ')                                                 ! ignore blank lines
      case('run','install','test','build','new','list','update','search')
          call execute_command_line( 'fpm '//linet )            ! call fpm
      case('fman')
          call execute_command_line( 'fpm '//linet(2:)//' --color|less -r' ) ! call fpm-man
      case('cd')
          call chdir(linet(ii+1:))                              ! gfortran fortran extension
      case('?')                                                 ! display help
              write(*,'(a)')help_text
      case('help')                                              ! display help
          call execute_command_line( 'fpm '//linet//'|more' )   ! call fpm
      case('man')                                               ! display help
          write(*,*)'env MANPATH=./man:"$MANPATH" '//linet
          call execute_command_line( 'env MANPATH="./man:$MANPATH" man '//linet )   ! call man with possible man directory
      case('version')
         call execute_command_line('fpm --version')             ! see if shell will execute it
      case('read')                                              ! read from alternate input file
          !!call dissect(linet(:ii),' -q F',linet(ii+1:),ierr)  ! define and parse command
          iin=iin+10                                            ! increment I/O unit used for input
          if(size(unnamed).gt.0)then
             cycle INFINITE
          elseif(unnamed(1).eq.'')then
             open(unit=iin,file=oldread,iostat=ios,iomsg=message,status='old')
          else
             oldread=sget('read')
             open(unit=iin,file=oldread,iostat=ios,iomsg=message,status='old')
          endif
          if(ios.ne.0)then
             iin=iin-10                                ! if open failed fall back to previous I/O unit
             write(*,*)'*read* ERROR:'//trim(message)
          endif
      case default
         !=============
         ! GNU Fortran (GCC) 6.4.0 -- fails on bad command if do not return CMDMSG
         ! call execute_command_line(linet(:len_trim(linet)))   ! see if shell will execute it
         !=============
         CMD: block
         integer            :: command_exit
         integer            :: command_status
         character(len=256) :: command_message
         command_message=''
         call execute_command_line(linet(:len_trim(linet)),exitstat=command_exit,cmdstat=command_status,cmdmsg=command_message)
         if(command_exit.ne.0)then
                 write(*,'(a)')trim(command_message)
         endif
         endblock CMD
      end select
   enddo INFINITE
   call journal('*fpm-shell* exiting')
contains

subroutine setup()
! @(#)help_usage(3f): prints help information
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   fpm-shell(1f) - shell for demonstrating interactive fpm concept              ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   shell --help| --version|[ -replay][ -read FILENAME]                          ',&
'DESCRIPTION                                                                     ',&
'    Example command line interface with command line history.                   ',&
'    Could add if/else/elseif/endif logic, calculator, basic graphics,           ',&
'    matlab88, ...                                                               ',&
'OPTIONS                                                                         ',&
'   --read FILENAME  read initial commands from specified file                   ',&
'   --replay         turn on journal file                                        ',&
'   --help           display this help and exit                                  ',&
'   --version        output version information and exit                         ',&
'USAGE                                                                           ',&
' At the command prompt the following example commands may be used:              ',&
'  #----------------------------------------------------------------------------#',&
'  | command                     || description                                 |',&
'  #----------------------------------------------------------------------------#',&
'  | run, build, test, help      || run "fpm CMD ..."                           |',&
'  | install, new, list, update  ||                                             |',&
'  | search, fman                || plugins which must be installed             |',&
'  | r                           || enter history editor; ? will produce help   |',&
'  | read file [-q]              || read from another input file                |',&
'  | ?                           || display this information                    |',&
'  | cd                          || change directory                            |',&
'  | version                     || fpm -version                                |',&
'  | quit|.                      || exit program                                |',&
'  | anything_else               | execute as system command                    |',&
'  #----------------------------------------------------------------------------#',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        fpm-shell(1f)>',&
'@(#)DESCRIPTION:    shell for demonstrating concept of interactive fpm>',&
'@(#)VERSION:        1.0.1, 20201213>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'']
end subroutine setup

end program fpm_shell
