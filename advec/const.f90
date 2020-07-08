module const

  implicit none

  !margin
  integer,parameter :: margin = 1

  !maximum steps
  integer,parameter :: nstop=1e6

  !interval of output data
  real(8),parameter :: dtout=1d0

  !end of time
  real(8),parameter :: tend=10d0

  !minimum
  real(8),parameter :: xmin = 0d0
  !maximum
  real(8),parameter :: xmax = 1d0

  !number of grid
  integer,parameter :: nx = 100+2*margin

  !grid size
  real(8),parameter :: dx0 = (xmax-xmin)/(nx-2*margin)

  !cfl number
  real(8),parameter :: cfl = 0.2d0

end module const
