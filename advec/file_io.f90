module file_io

    implicit none

	character,private :: cno*4

    contains

    subroutine file_output(qq,nx,margin,nd)

	integer,intent(in) :: nx,margin,nd
	real(8),dimension(nx),intent(in) :: qq

	write(cno,'(i4.4)') nd
	open(10,file='data/'//cno//'_qq.txt',status='replace')
	write(10,*)qq(1+margin:nx-margin)
	close(10)

    end subroutine file_output

end module file_io
