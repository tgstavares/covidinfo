module values
  use prec; use parameters
contains

  real(dp) function utility(n)
    implicit none
    real(dp), intent(in)::n

    utility = log(n) +  lambda_p*log(1d0-n) + b
    !utility =n*(1d0-n)**lambda_p * b
    
  end function utility

  real(dp) function utilityi(n)
    implicit none
    real(dp), intent(in)::n
    
    utilityi = log(n) + lambda_pp*log(1d0-n) + b
    !utilityi =n*(1d0-n)**lambda_pp * b
    
  end function utilityi

  
end module values
