module globals
  use prec

  ! model variables
  type model
     real(dp) vi,vc,vr,vw1
     real(dp), allocatable, dimension(:)::vs,ppi_0,ppi_b,ppi,ppie,mdr,mdf,delay
     real(dp), allocatable, dimension(:)::ns, nagg, ms ,mi ,mc ,mr ,md, mw1,mw2,mw3,mw4
     real(dp), allocatable, dimension(:)::nse,mse,mie,mce,mre,mde, mw1e,mw2e,mw3e,mw4e
     real(dp), allocatable, dimension(:)::nsb,msb,mib,mcb,mrb,mdb, mw1b,mw2b,mw3b,mw4b
  end type model

  real*8 t1,t2
end module globals
