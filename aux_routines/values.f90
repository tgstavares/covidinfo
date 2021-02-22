module values
  use prec; use parameters
contains
  real(dp) function utility(n)
    implicit none
    real(dp), intent(in)::n
    utility = log(n) +  lambda_p*log(1d0-n) + b
  end function utility  
end module values
