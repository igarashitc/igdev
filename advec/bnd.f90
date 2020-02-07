module bnd

implicit none

contains

  subroutine free_bnd(qq,margin,nx)

    real(8),dimension(nx),intent(inout) :: qq
    integer,intent(in) :: margin,nx

    qq(margin) = qq(margin+1)
    qq(nx)     = qq(nx-margin)

  end subroutine free_bnd

  subroutine periodic_bnd(qq,margin,nx)

    real(8),dimension(nx),intent(inout) :: qq
    integer,intent(in) :: margin,nx

    qq(margin) = qq(nx-margin)
    qq(nx)     = qq(margin+1)

  end subroutine periodic_bnd

end module bnd
