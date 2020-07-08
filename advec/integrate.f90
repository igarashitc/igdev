module integrate

    use bnd

    implicit none

contains

    subroutine integrate_ftcs(qq,cc,x,dx,nx,dt,margin,cfl)

	    integer,intent(in) :: nx
	    real(8),dimension(nx),intent(in) :: x,dx

	    real(8),dimension(nx),intent(inout) :: qq             
	    real(8),dimension(nx),intent(in) :: cc
	    real(8),dimension(nx) :: qq0

	    real(8),intent(inout) :: dt
	    real(8),intent(in) :: cfl

	    integer,intent(in) :: margin

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
	    call periodic_bnd(qq,margin,nx)

    end subroutine integrate_ftcs

    subroutine integrate_upwind(qq,cc,x,dx,nx,dt,margin,cfl)

	    integer,intent(in) :: nx
	    real(8),dimension(nx),intent(in) :: x,dx

	    real(8),dimension(nx),intent(inout) :: qq
	    real(8),dimension(nx),intent(in) :: cc
	    real(8),dimension(nx) :: qq0

	    real(8),intent(inout) :: dt
	    real(8),intent(in) :: cfl

	    integer,intent(in) :: margin

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
	    call periodic_bnd(qq,margin,nx)

    end subroutine integrate_upwind

    subroutine integrate_lw(qq,cc,x,dx,nx,dt,margin,cfl)

	integer,intent(in) :: nx
	real(8),dimension(nx),intent(in) :: x,dx

	real(8),dimension(nx),intent(inout) :: qq             
	real(8),dimension(nx),intent(in) :: cc
	real(8),dimension(nx) :: qq0

	real(8),intent(inout) :: dt
	real(8),intent(in) :: cfl

	integer,intent(in) :: margin

	integer :: i

	qq0 = qq

	!calcurate delta t
	  do i=margin+1,nx-margin
	      dt = min(dt,cfl*dx(i)/cc(i))
	  enddo

	!integrate by lw(lax-wendroff scheme)
	do i=margin+1,nx-margin
	    qq(i) = qq0(i) &
		- 0.5d0*cc(i)*dt*(qq0(i+1) - qq0(i-1))/dx(i) &
		+ 0.5d0*cc(i)*cc(i)*dt*dt*(qq0(i+1)-2d0*qq0(i)+qq0(i-1))/(dx(i)*dx(i))
	enddo
	call periodic_bnd(qq,margin,nx)

    end subroutine integrate_lw

    subroutine integrate_upwind_conserve(qq,cc,x,dx,nx,dt,margin,cfl)

	integer,intent(in) :: nx
	real(8),dimension(nx),intent(in) :: x,dx

	real(8),dimension(nx),intent(inout) :: qq             
	real(8),dimension(nx),intent(in) :: cc
	real(8),dimension(nx) :: qq0
	real(8),dimension(nx) :: qqf

	real(8),intent(inout) :: dt
	real(8),intent(in) :: cfl

	integer,intent(in) :: margin

	integer :: i

	!calcurate delta t
	do i=margin+1,nx-margin
	    dt = min(dt,cfl*dx(i)/cc(i))
	enddo

      qq0 = qq

      do i=1,nx
	qqf(i) = 0.5d0*(qq(i+1)*cc(i+1)+qq(i)*cc(i) - abs(cc(i))*(qq(i+1)-qq(i)))
      enddo

      !integrate by upwind
      do i=margin+1,nx-margin
	qq(i) = qq0(i) - dt*(qqf(i+1) - qqf(i))/dx(i)
      enddo
      call periodic_bnd(qq,margin,nx)

    end subroutine integrate_upwind_conserve

end module integrate
