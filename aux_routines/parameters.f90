module parameters
  use prec

  ! Grids
  integer, parameter:: &
       tt    = 500, &
       tvacc = 400, &
       tlag  = 31, &
       nvfi  = 800
  
  ! Parameters
  real(dp), parameter:: &       
       lambda_p	= 1.77d0, &
       n_bar    = 1d0/(1d0+lambda_p), &
       lf_sick  = 0.5d0, &
       b      	= 11.4d0, &
       beta     = 0.98**(1d0/365d0), &
       delta    = 0.005d0, &
       ppi_0    = 4.627d0, &
       initinf  = 0.000001d0, &
       gamma    = 0.166667d0, &
       pop      = 120d6

  ! Control parameters - small and large numbers
  real(dp), parameter:: &
       infty = 1.0d10, &
       erro3 = 1.0d-3, &
       erro4 = 1.0d-5, &
       erro5 = 1.0d-5, &
       erro6 = 1.0d-6, &
       erro7 = 1.0d-7, &
       erro8 = 1.0d-8, &
       erro9 = 1.0d-9, &
       erro10= 1.0d-10, &
       erro12= 1.0d-12

end module parameters
