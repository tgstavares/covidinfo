module globals
  use prec

  ! model variables
  type model
     real(dp) n_bar,n_bari,vi,vr,vc
     real(dp), allocatable, dimension(:)::vs,ns,ppi,ppi_tilde,ms,mi,mr,mc,md,nc,ppif
     integer iter
  end type model

  integer tlag
  real*8 t1,t2
end module globals
