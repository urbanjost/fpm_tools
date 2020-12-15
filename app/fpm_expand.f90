program expand
use M_CLI, only : commandline, check_commandline, files=>unnamed
use M_strings, only: notabs, split
implicit none

character(len=*),parameter::ident_1="@(#)fpm_expand(1f):filter removes tabs and trailing white space on files up to 1024 chars wide"

character(len=1024)              :: in,out
integer                          :: ios          ! error flag from read
integer                          :: iout
integer                          :: i,ii
integer                          :: iblanks

integer                      :: blanks; namelist /args/ blanks
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
logical                      :: usage=.FALSE.; namelist  /args/usage !! special namelist when no arguments

character(len=*),parameter :: cmd='-blanks -1'

   call set()
   CMDLINE : block
      character(len=255)           :: message
      character(len=:),allocatable :: readme
      integer                      :: ios
      !! SET ALL DEFAULT VALUES AND THEN APPLY VALUES FROM COMMAND LINE
      readme=commandline(cmd)
      read(readme,nml=args,iostat=ios,iomsg=message) !! UPDATE NAMELIST VARIABLES
      call check_commandline(ios,message,help_text,version_text)     !! HANDLE ERRORS FROM NAMELIST READ
   endblock CMDLINE

   if(size(files).eq.0)then                                ! default is to read from stdin, which the filename "-" designates
      files=['-']
   endif

   ALLFILES: do i=1,size(files)                            ! loop through all the filenames
      if(files(i).eq.'-'.or.files(i).eq.'stdin')then       ! special filename designates stdin
         ii=5
      else                                                 ! open a regular file
         ii=20
         open(unit=ii,file=trim(files(i)),iostat=ios,status='old',form='formatted')
         if(ios.ne.0)then
            !call stderr('*fpm_expand* failed to open:'//trim(files(i)))
            cycle ALLFILES
         endif
      endif
      iblanks=0
      ALLLINES: do                                         ! loop thru the file and call notabs(3f) on each line
         read(ii,"(a)",iostat=ios)in
         if(ios /= 0)then
            exit ALLLINES
         endif
         call notabs(in,out,iout)
         if(blanks.ge.0)then
            if(out(:iout).eq.'')then
               iblanks=iblanks+1
               if(iblanks.gt.blanks)then
                  cycle ALLLINES
               endif
            else
               iblanks=0
            endif
         endif
         write(*,"(a)")out(:iout)
      enddo ALLLINES
      close(unit=20,iostat=ios)
   enddo ALLFILES

contains

subroutine set()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       fpm-expand(1f) - [FUNIX] convert tabs to spaces                          ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       fpm-expand FILENAME(S) [ -blanks N]| --help| --version                   ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Convert tabs in each FILE to spaces, writing to standard output.         ',&
'       If no filename is specified standard input is read. Tab stops            ',&
'       are assumed to be every eight (8) columns. Trailing spaces,              ',&
'       carriage returns, and newlines are removed.                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       FILENAMES   files to expand tab characters in.                           ',&
'       -blanks     maximum number of adjacent blank lines to retain.            ',&
'                   Default is -1, which is equivalent to unlimited.             ',&
'       --help      display this help and exit                                   ',&
'       --version   output version information and exit                          ',&
'       --usage     basic usage information including a list of arguments        ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        fpm-expand < input.txt > output.txt                                     ',&
'        fpm-expand input.txt   > output.txt                                     ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PROGRAM:        fpm-expand(1f)>',&
'@(#)DESCRIPTION:    convert tabs to spaces>',&
'@(#)VERSION:        1.0, 20151220>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'']
end subroutine set

end program expand
