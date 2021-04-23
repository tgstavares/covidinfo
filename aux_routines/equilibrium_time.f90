module equilibrium
  use prec; use parameters; use values
contains

  subroutine get_equil(tu,ppi_belief,ns,vs,ppi,ms,mw1,mw2,mw3,mw4,mi,mc,mr,md,showoutput)
    implicit none
    integer, intent(in)::tu
    real(dp), intent(in):: ppi_belief
    real(dp), intent(inout):: ns(tt),vs(tt),ppi(tt)
    real(dp), intent(out):: ms(tt+1),mw1(tt+1),mw2(tt+1),mw3(tt+1),mw4(tt+1),mi(tt+1),mc(tt+1),mr(tt+1),md(tt+1)
    character(len=1), intent(in), optional::showoutput
    integer flag_showoutput,iter,t
    real(dp) diff,vr,vc,vi,vw1,vacc,aux,v1,v0,n0,ppit0,ppif(tt)

    flag_showoutput = 1
    if(present(showoutput))then
       if(showoutput(1:1).eq.'n'.or.showoutput(1:1).eq.'N') flag_showoutput = 0
    end if

    ! INITIAL GUESS
    do t=1,tt
       if(t.ge.tvacc) then
          vacc     = (1d0-1d0/dble(t-tvacc+1)) * 0d0
       else
          vacc  = 1d0
       end if
       ppi(t) = (0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1))*vacc
    end do
    ppif = 0d0

    ! COMPUTE INVARIABLE VARIABLES AND VALUES
    vr = utility(n_bar) / (1d0 - beta)
    vc =(utility(n_bar*lf_sick) + beta*theta*(1d0-delta)*vr) / (1d0 - beta*(1d0-theta))
    vi =(utility(n_bar*lf_sick) + beta*gamma*vc) / (1d0 - beta*(1d0-gamma))
    vw1= utility(n_bar)*(1d0+beta+beta**2d0+beta**3d0) + beta**4d0 * vi
    
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
          aux  = beta*ppit0*(v1-vw1)
          if(aux.gt.erro7)then        
             n0   = ((1d0+lambda_p+aux) - sqrt((1d0+lambda_p+aux)**2d0 - 4d0*aux)) / (2d0*aux)
             v0   = utility(n0) + beta * ( (1d0-n0*ppit0)*v1 + (n0*ppit0)*vw1 )
             if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
          else
             n0   = n_bar
             v0   = utility(n0) + beta* ( (1d0-n0*ppit0)*v1 + beta*(n0*ppit0)*vw1 )
             if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
          end if

          ! UPDATE VALUES AND LABOR SUPPLY
          vs(t) = v0
          ns(t) = n0
       end do

       ! COMPUTE TRANSITIONS
       call update_masses(tu,tt,ppi_belief,ns,ppif,ms,mw1,mw2,mw3,mw4,mi,mc,mr,md)

       ! criterium
       diff = maxval(abs(ppi-ppif))
       if((flag_showoutput.eq.1)) then
          !if(mod(iter,20).eq.0) write(*,'(i5,f35.10)'),iter,diff
          if(mod(iter,20).eq.0) print*,iter,diff
       end if
       if(iter.ge.nvfi)then
          print*,'NO CONVERGENCE'
          stop
          exit
       end if
       ppi = ppi + 0.1d0*(ppif-ppi)
    end do
  end subroutine get_equil

  subroutine update_masses(tb,tu,ppi_belief,ns,ppif,ms,mw1,mw2,mw3,mw4,mi,mc,mr,md)
    implicit none
    integer, intent(in)::tb,tu
    real(dp), intent(in)::ppi_belief,ns(tt)
    real(dp), intent(inout)::ppif(tt),ms(tt+1),mw1(tt+1),mw2(tt+1),mw3(tt+1),mw4(tt+1),mi(tt+1),mc(tt+1),mr(tt+1),md(tt+1)
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
       mw1(t+1)= ms(t) * ns(t) * ppif(t)
       mw2(t+1)= mw1(t)
       mw3(t+1)= mw2(t)
       mw4(t+1)= mw3(t)
       mi(t+1) = mi(t) - mi(t) * gamma + mw4(t)
       mc(t+1) = mc(t) - mc(t) * theta + mi(t) * gamma
       mr(t+1) = mr(t) + mc(t) * theta * (1d0 - delta)
       md(t+1) = md(t) + mc(t) * theta * delta
    end do
    !print*,tb,tu    
  end subroutine update_masses

  subroutine recov_masses(tb,tu,ppi,vacc,d,n,s0,w10,w20,w30,w40,i0,c0,r0)
    implicit none
    integer, intent(in)::tb,tu
    real(dp), intent(in)::ppi(tb-tu:0),vacc(tb-tu:0),d(tb-tu:0),n(tb-tu:-1)
    real(dp), intent(out)::s0,i0,c0,r0,w10,w20,w30,w40
    real(dp) c(tb-tu:0),i(tb-tu:0),s(tb-tu:0),ppif(tb-tu:0),recov,w1(tb-tu:0),w2(tb-tu:0),w3(tb-tu:0),w4(tb-tu:0)
    integer t    
    ! MOVE BACKWARD
    c(-1)    = (d( 0)-d(-1))/(theta*delta)
    c(-2)    = (d(-1)-d(-2))/(theta*delta)
    c(-3)    = (d(-2)-d(-3))/(theta*delta)
    c(-4)    = (d(-3)-d(-4))/(theta*delta)
    c(-5)    = (d(-4)-d(-5))/(theta*delta)
    c(-6)    = (d(-5)-d(-6))/(theta*delta)
    i(-2)    = (c(-1) - c(-2) + c(-2)*theta) / gamma
    i(-3)    = (c(-2) - c(-3) + c(-3)*theta) / gamma
    i(-4)    = (c(-3) - c(-4) + c(-4)*theta) / gamma
    i(-5)    = (c(-4) - c(-5) + c(-5)*theta) / gamma
    i(-6)    = (c(-5) - c(-6) + c(-6)*theta) / gamma
    w4(-6)   = i(-5) - (1d0-gamma)*i(-6)
    w4(-5)   = i(-4) - (1d0-gamma)*i(-5)
    w4(-4)   = i(-3) - (1d0-gamma)*i(-4)
    w4(-3)   = i(-2) - (1d0-gamma)*i(-3)
    w3(-6)   = w4(-5)
    w3(-5)   = w4(-4)
    w3(-4)   = w4(-3)
    w2(-6)   = w3(-5)
    w2(-5)   = w3(-4)
    w1(-6)   = w2(-5)    
    if(d(-6).gt.0d0)then
       recov = d(-6)*(1d0-delta)/delta
    else
       recov = 0d0
    end if
    s(-6) = 1d0 - (d(-6) + recov + c(-6) + i(-6) + w4(-6) + w3(-6) + w2(-6) + w1(-6))  
    ! MOVE FORWARD
    do t=-6,-1
       ppif(t) = 1d0 - exp(-ppi(t)*i(t)*n_bar*lf_sick*vacc(t))
       s(t+1) = s(t) * (1d0 - n(t) * ppif(t))
       w1(t+1)= s(t) * n(t) * ppif(t)
       w2(t+1)= w1(t)
       w3(t+1)= w2(t)
       w4(t+1)= w3(t)
       i(t+1) = i(t) - i(t) * gamma + w4(t)
       c(t+1) = c(t) - c(t) * theta + i(t) * gamma
    end do
    s0 = s(0)
    w10= w1(0)
    w20= w2(0)
    w30= w3(0)
    w40= w4(0)
    i0 = i(0)
    c0 = c(0)
    r0 = 1d0  - (s(0) + w1(0) + w2(0) + w3(0) + w4(0) + i(0) + c(0) + d(0))
  end subroutine recov_masses
  
  subroutine deaths_reported(thist,pprob,tu,md,mdr_p,mdr,delay)
    implicit none
    integer, intent(in)::thist
    real(dp), intent(in)::pprob(thist)
    integer, intent(in)::tu
    real(dp), intent(in)::md(tu),mdr_p
    real(dp), intent(out)::mdr,delay
    integer j
    real(dp) soma


    !print*,'------'
    soma = 0d0
    do j=1,min(thist,tu)
       soma = soma + pprob(j)*(md(tu-j+1)-md(max(tu-j,1)))
       !print*,tu,tu-j+1,max(tu-j,1),pprob(j),(md(tu-j+1)-md(max(tu-j,1)))
    end do    
    !print*,tu,soma,(1d0/dble(tlag)) * (md(tu) - md(max(tu-tlag,1)))
    !stop 'hey!!!'
    mdr = mdr_p + soma

    !mdr = (1d0/dble(tlag)) * (md(tu) - md(max(tu-tlag,1))) + mdr_p

    !dearths_rep_t == 1/5 D.Dt + 1/5 D.Dt-1 ... + 1/5 D.Dt-5 + deaths_rep_t-1
    
    !mdr = md(max(tu-tlag+1,1))
    
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
