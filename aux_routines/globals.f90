module globals
  use prec

  ! model variables
  type model
     real(dp) vi,vr
     real(dp), allocatable, dimension(:)::vs,ns,nse,ms,mi,mr,md,ppi,mdr,mdf,delay
  end type model

  real*8 t1,t2
end module globals
