module equilibrium
  use prec; use parameters; use values
contains

  subroutine get_equil(tu,ppi_belief,ns,vs,ppi,ms,mi,mr,md,showoutput)
    implicit none
    integer, intent(in)::tu
    real(dp), intent(in):: ppi_belief
    real(dp), intent(inout):: ns(tt),vs(tt),ppi(tt)
    real(dp), intent(out):: ms(tt+1),mi(tt+1),mr(tt+1),md(tt+1)
    character(len=1), intent(in), optional::showoutput
    integer flag_showoutput,iter,t
    real(dp) diff,vr,vi,vacc,aux,v1,v0,n0,ppit0,ppif(tt)

    flag_showoutput = 1
    if(present(showoutput))then
       if(showoutput(1:1).eq.'n'.or.showoutput(1:1).eq.'N') flag_showoutput = 0
    end if

    ! COMPUTE INVARIABLE VARIABLES AND VALUES
    vr    = utility(n_bar) / (1d0 - beta)
    vi    = (utility(n_bar*lf_sick) + beta*gamma*(1d0-delta)*vr) / (1d0 - beta*(1d0-gamma))
    
    diff = 1d0
    iter = 0
    do while(diff.gt.erro9)
       iter = iter + 1

       ! FINAL VALUES AND INITIAL STATES
       vs(tt)    = vr
       ns(tt)    = n_bar
       
       ! COMPUTE OPTIMAL POLICIES AND VALUES
       do t=tt-1,tu,-1
          if(t.ge.tvacc) then
             vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
          else
             vacc = 1d0
          end if

          ! BELIEF
          ppit0    = ppi(t) * vacc

          ! SOLVING LABOR SUPPLY
          v1   = vs(t+1)      
          aux  = beta*ppit0*(v1-vi)
          if(aux.gt.erro7)then        
             n0   = ((1d0+lambda_p+aux) - sqrt((1d0+lambda_p+aux)**2d0 - 4d0*aux)) / (2d0*aux)
             v0   = utility(n0) + beta * ( (1d0-n0*ppit0)*v1 + (n0*ppit0)*vi )
             if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
          else
             n0   = n_bar
             v0   = utility(n0) + beta* ( (1d0-n0*ppit0)*v1 + beta*(n0*ppit0)*vi )
             if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
          end if

          ! UPDATE VALUES AND LABOR SUPPLY
          vs(t) = v0
          ns(t) = n0
       end do
       
       ! COMPUTE TRANSITIONS
       call update_masses(tu,tt,ppi_belief,ns(1:tt),ppif(1:tt),ms(1:tt+1),mi(1:tt+1),mr(1:tt+1),md(1:tt+1))

       ! criterium
       diff = maxval(abs(ppi-ppif))
       if((flag_showoutput.eq.1)) then
          if(mod(iter,20).eq.0) write(*,'(i5,f15.10)'),iter,diff
       end if
       if(iter.ge.nvfi)then
          print*,'NO CONVERGENCE'
          exit
       end if
       ppi = ppi + 0.1d0*(ppif-ppi)
    end do
  end subroutine get_equil

  subroutine update_masses(tb,tu,ppi_belief,ns,ppif,ms,mi,mr,md)
    implicit none
    integer, intent(in)::tb,tu
    real(dp), intent(in)::ppi_belief,ns(tt)
    real(dp), intent(inout)::ppif(tt),ms(tt+1),mi(tt+1),mr(tt+1),md(tt+1)
    integer t
    real(dp) vacc    
    do t=tb,tu
       if(t.ge.tvacc) then
          vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
       else
          vacc = 1d0
       end if
       ppif(t) = 1d0 - exp(-ppi_belief*mi(t)*n_bar*lf_sick*vacc)       
       ! UPDATE MASSES
       ms(t+1) = ms(t) * (1d0 - ns(t) * ppif(t))
       mi(t+1) = mi(t) * (1d0 - gamma) + ms(t) * ns(t) * ppif(t)
       mr(t+1) = mr(t) + mi(t) * gamma * (1d0 - delta)
       md(t+1) = md(t) + mi(t) * gamma * delta
    end do
  end subroutine update_masses

  subroutine deaths_reported(tu,md,mdr_p,mdr,delay)
    implicit none
    integer, intent(in)::tu
    real(dp), intent(in)::md(tu),mdr_p
    real(dp), intent(out)::mdr,delay
    integer j
    real(dp) soma

    ! if(tu.le.1)then
    !    mdr = md(tu)
    ! else
    !    soma = 0d0
    !    do j=1,min(tu-1,tlag-1)
    !       soma = soma + dble(j)*(md(tu-j+1)-md(tu-j))
    !    end do
    !    mdr = (1d0/dble(tlag)) * soma + md(max(tu-tlag+1,1))       
    ! end if

    mdr = (1d0/dble(tlag)) * (md(tu) - md(max(tu-tlag,1))) + mdr_p
    !mdr = md(max(tu-10,1))

    
    delay = 0d0
    j = 0
    if(mdr+erro9.lt.md(tu-j))then
       do while(mdr.lt.md(tu-j))
          j = j + 1
       end do
       delay = dble(j) - (mdr-md(tu-j)) / (md(tu-j+1) - md(tu-j))
    end if
  end subroutine deaths_reported
  
end module equilibrium
