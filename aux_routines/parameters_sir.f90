module parameters
  use prec

  ! Grids
  integer, parameter:: &
       tt    = 501, &
       tvacc = 350, &
       tlag  = 1, &
       tscr  = 0, &
       tppi_0= 1500, &
       tppi_1= 10001, &
       nvfi  = 800, &

       s3    = 12503	! seed for random variable
  
  ! Parameters
  real(dp), parameter:: &       
       lambda_p	= 1.77d0, &
       n_bar    = 1d0/(1d0+lambda_p), &
       lf_sick  = 0.5d0, &
       b      	= 6.5d0, &
       beta     = 0.98**(1d0/365d0), &
       delta    = 0.008d0, &
       theta    = 0.10d0, &
       gamma    = 0.166667d0, &
       ppi_0    = 5.11d0, &
       initinf  = 0.000001d0, &       
       pop      = 120d6, &

       sppi_0   = 1.3d0, &
       
       r0       = 2d0


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
