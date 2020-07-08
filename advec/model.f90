module model

    use const
    use bnd

    implicit none
    private

    public :: model_setup

    real(8),public,dimension(nx) :: x
    real(8),public,dimension(nx) :: dx
    
    real(8),public,dimension(nx) :: qq

    real(8),public,dimension(nx) :: cc

    real(8),public :: dt

contains      

  subroutine model_setup(nx,margin,xmin,dx0)
    integer,intent(in) :: margin
    integer,intent(in) :: nx

    real(8),intent(in) :: xmin,dx0

    integer :: i

    !!!-------------------------
    !!! set geometry
    !!!-------------------------
    do i=1,nx
	dx(i) = dx0
    enddo

    x(2) = xmin
    do i=margin+1,nx-margin
	x(i+1) = x(i) + dx(i)
    enddo 
    x(nx) = x(nx-margin)+dx(nx)

    open(10,file='data/x.txt',status='replace')
    write(10,*)x(margin+1:nx-margin)
    close(10)
    write(*,*)nx

    !!!-------------------------
    !!! set physical quantity
    !!!-------------------------
    do i=margin+1,nx-margin
!                    if(x(i) .le. .25 .or. x(i) .ge. .75)then
!                        qq(i) = 0d0
!                    else     
!                        qq(i) = 1d0
!                    endif
    if (x(i) .le. .5)then
    qq(i) = 1d0
    else
    qq(i) = 0d0
    endif
!		    qq(i) = sin(x(i))
	      cc(i) = 1d0 
	    enddo

    call periodic_bnd(qq,margin,nx)

	    dt = 1e4

   end subroutine model_setup

end module model
