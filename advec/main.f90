program main

    use const 
    use model
    use integrate
    use file_io

    implicit none

    !time in simulation
    real(8) :: time,timep
    !number of steps
    integer :: ns
    !control variables
    integer :: nt1,nt2
    integer :: nd
    integer :: mw

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    time = 0d0
    nd = 0

    call model_setup(nx,margin,xmin,dx0)
    call file_output(qq,nx,margin,nd)

    loop: do ns=1,nstop
!	call integrate_ftcs(qq,cc,x,dx,nx,dt,cfl)
	call integrate_upwind(qq,cc,x,dx,nx,dt,cfl)

	timep = time
	time = time+dt

	!data output!
	nt1 = int(timep/dtout)
	nt2 = int(timep/dtout)
	if(nt1 < nt2) mw=1
	if(mw /= 0)then
	    write(*,*)nd,time,dt
	    nd = nd+1
	    call file_output(qq,nx,margin,nd)
	endif

	if (time .gt. tend) exit loop
    enddo loop

    write(*,*) "The time is over, steps: ", ns

end program



