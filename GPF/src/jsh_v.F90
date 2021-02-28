
!-----------------------------------------------------------------------
subroutine jsh_v(i) ! assume long routine names are supported
implicit none
integer,intent(in) :: i
      ! return version number in character variable version and print
      ! compile information to unit i
      if(i.ge.0)then
      write(i,'(1x,79("-"))')
      call trimit('@(#)File ................ jsh_v>')
      call trimit('@(#)Program Version ..... 0.0.0>')
      call trimit('@(#)Build Target ........ Linux_ifort>')
      call trimit('@(#)Compile Date ........ '//&
     &'Thu Dec 31 23:50:45 EST 2020>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'localhost.localdomain>')
      call trimit('@(#) System Type ........ '// &
     &'Linux>')
      call trimit('@(#) O.S. Release ....... '// &
     &'4.18.0-193.1.2.el8_2.x86_64>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'#1 ' &
     &//'SMP ' &
     &//'Thu ' &
     &//'May ' &
     &//'7 ' &
     &//'16:37:54 ' &
     &//'UTC ' &
     &//'2020 ' &
     &//'>')
      call trimit('@(#) Hardware Name ...... '//&
     &'x86_64>')
      write(i,'(1x,79("-"))')
      endif
      contains
      subroutine trimit(string) ! leave off metadata prefix
      character(len=*) :: string
         write(i,*)trim(string(5:len_trim(string)-1))
      end subroutine trimit
end subroutine jsh_v
!-----------------------------------------------------------------------
