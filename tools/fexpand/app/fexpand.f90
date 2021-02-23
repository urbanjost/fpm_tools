program fexpand
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use M_CLI2,    only : set_args, iget, files=>unnamed
use M_strings, only : dilate 
use M_io,      only : getline
implicit none

!character(len=*),parameter::ident_1="@(#)fexpand(1f):filter removes tabs and trailing white space on files "

character(len=:),allocatable  :: line
character(len=:),allocatable  :: out
integer                       :: ios          ! error flag from read
integer                       :: i,ii
integer                       :: iblanks
integer                       :: blanks
integer                       :: width
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)

character(len=*),parameter :: cmd='-blanks -1'

   call setup()
   call set_args('-blanks:b -1 --width:w 132',&
   width=iget('width')
   blanks=iget('blanks')

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
            !call stderr('*fexpand* failed to open:'//trim(files(i)))
            cycle ALLFILES
         endif
      endif
      iblanks=0
      open(unit=ii,pad='yes')
      ALLINES: do while (getline(line)==0)
         line=dilate(line)
         if(blanks.ge.0)then
            if(line.eq.'')then
               iblanks=iblanks+1
               if(iblanks.gt.blanks)then
                  cycle ALLINES
               endif
            else
               iblanks=0
            endif
         endif
         if(len(line).gt.width)then
         endif
         write(*,"(a)")line
      enddo ALLINES
      close(unit=ii,iostat=ios)
   enddo ALLFILES
contains

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       fexpand(1f) - [FUNIX] convert tabs to spaces                             ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       fexpand FILENAME(S) [ -blanks NNN][ --width ]| --help| --version         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Convert tabs in each FILE to spaces, writing to standard output.         ',&
'       If no filename is specified standard input is read. Tab stops            ',&
'       are assumed to be every eight (8) columns. Trailing spaces,              ',&
'       carriage returns, and newlines are removed.                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       FILENAMES   files to expand tab characters in.                           ',&
        --width     line width to produce a warning if exceeded. Defaults        ',&
                    to 132. If less then or equal to 0 no warnings are produced. ',&
'       --blanks     maximum number of adjacent blank lines to retain.           ',&
'                   Default is -1, which is equivalent to unlimited.             ',&
'       --help      display this help and exit                                   ',&
'       --version   output version information and exit                          ',&
'       --usage     basic usage information including a list of arguments        ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        fexpand < input.txt > output.txt                                        ',&
'        fexpand input.txt   > output.txt                                        ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PROGRAM:      fexpand(1f)>',&
'@(#)DESCRIPTION:  convert tabs to spaces>',&
'@(#)VERSION:      1.0, 20151220>',&
'@(#)AUTHOR:       John S. Urban>',&
'@(#)LICENSE:      MIT>',&
'']
end subroutine setup

end program fexpand
