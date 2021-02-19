module parameters
  use prec

  ! Grids
  integer, parameter:: &
       tt = 5001, &
       tl1= 0, &
       tl2= 6, &
       tl3= 9, &
       tl4= 15
  
  ! Parameters
  real(dp), parameter:: &       
       lambda_p	= 1.77d0, &
       lambda_pp= 1.77d0, &

       !b      	= 7.4d0, & !3.5
       b      	= 7.4d0, & !3.5
       
       !beta     = 0.98**(1d0/365d0), & !0.95
       beta     = 0.98**(1d0/365d0), & !0.95
       
       delta    = 0.008d0, &
       
       !ppi_0    = 2.353d0, &
       ppi_0    = 2.353d0, &

       !initinf  = 0.00001d0, &
       initinf  = 0.00001d0, &

       theta    = 0.1d0, &
       gamma    = 0.166667d0, &
       popfact  = 120d6

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
