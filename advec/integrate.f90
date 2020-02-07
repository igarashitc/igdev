module integrate

      use const

      implicit none

contains

      subroutine integrate_ftcs(qq,cc,x,dx,nx,dt,cfl)

              integer,intent(in) :: nx

              real(8),dimension(nx),intent(in) :: x,dx

              real(8),dimension(nx),intent(inout) :: qq
             
	      real(8),dimension(nx),intent(in) :: cc

              real(8),dimension(nx) :: qq0

	      real(8),intent(inout) :: dt

	      real(8),intent(in) :: cfl

              integer :: i

              qq0 = qq

	      !calcurate delta t
		do i=margin+1,nx-margin
		    dt = min(dt,cfl*dx(i)/cc(i))
		enddo

	      !integrate by FTCS (Forward in Time and Centered difference in Space)
              do i=margin+1,nx-margin
                  qq(i) = qq0(i) - 0.5d0*cfl*(qq0(i+1) - qq0(i-1))
              enddo

      end subroutine integrate_ftcs

      subroutine integrate_upwind(qq,cc,x,dx,nx,dt,cfl)

              integer,intent(in) :: nx

              real(8),dimension(nx),intent(in) :: x,dx

              real(8),dimension(nx),intent(inout) :: qq
             
	      real(8),dimension(nx),intent(in) :: cc

              real(8),dimension(nx) :: qq0

	      real(8),intent(inout) :: dt

	      real(8),intent(in) :: cfl

              integer :: i

              qq0 = qq

	      !calcurate delta t
		do i=margin+1,nx-margin
		    dt = min(dt,cfl*dx(i)/cc(i))
		enddo

	      !integrate by up-wind scheme
              do i=margin+1,nx-margin
                  qq(i) = qq0(i) - cc(i)*dt*(qq0(i) - qq0(i-1))/dx(i)
              enddo

      end subroutine integrate_upwind

end module integrate