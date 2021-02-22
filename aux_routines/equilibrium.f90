module equilibrium
  use prec; use parameters
contains

  subroutine get_equil(ppi)
    implicit none
    real(dp), intent(inout):: ppi(tt)
    real(dp), intent(out):: ms(tt+1),mi(tt+1),mr(tt+1),
    real(dp) n_bar,vr,vi
    
    ! COMPUTE INVARIABLE VARIABLES AND VALUES
    n_bar = 1d0/(1d0+lambda_p)
    vr    = utility(n_bar) / (1d0 - beta)
    vi    = (utility(n_bar*lf_sick) + beta*gamma*(1d0-delta)*vr) / (1d0 - beta*(1d0-gamma))

    ms(1) = 1d0 - initinf
    mi(1) = 1d0 - m%ms(1)
    mr(1) = 0d0
    md(1) = 0d0


  end subroutine get_equil
  
end module equilibrium
