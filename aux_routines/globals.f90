module globals
  use prec

  ! model variables
  type model
     real(dp) n_bar,vi,vr
     real(dp), allocatable, dimension(:)::vs,ns,ms,mi,mr,md,ppi,ppif
     integer iter
  end type model

  integer tlag
  real*8 t1,t2
end module globals
