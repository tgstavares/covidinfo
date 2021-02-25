module globals
  use prec

  ! model variables
  type model
     real(dp) vi,vc,vr
     real(dp), allocatable, dimension(:)::vs,ppi_0,ppi_b,ppi,ppie,mdr,mdf,delay
     real(dp), allocatable, dimension(:)::ns ,ms ,mi ,mc ,mr ,md
     real(dp), allocatable, dimension(:)::nse,mse,mie,mce,mre,mde
     real(dp), allocatable, dimension(:)::nsb,msb,mib,mcb,mrb,mdb
  end type model

  real*8 t1,t2
end module globals
