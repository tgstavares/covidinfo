program main
  use prec; use parameters; use omp_lib; use globals; use values
  implicit none

  type(model) m
  integer t,i,tp
  real(dp) diff,n0,n1,aux,v1,v0,ppit0,vacc,ppi_belief
  real(dp) diff_deaths(tt),agg_hours(tt)

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()

  allocate( &
       m%vs(tt),m%ns(tt), &
       m%ppi(tt),m%ppif(tt), &
       m%ms(tt+1),m%mi(tt+1),m%mr(tt+1),m%md(tt+1))


  ! COMPUTE INVARIABLE VARIABLES AND VALUES
  m%n_bar = 1d0/(1d0+lambda_p)
  m%vr    = utility(m%n_bar) / (1d0 - beta)
  m%vi    = (utility(m%n_bar*lf_sick) + beta*gamma*(1d0-delta)*m%vr) / (1d0 - beta*(1d0-gamma))

  ! INITIAL GUESS
  do t=1,tt
     if(t.ge.tvacc) then
        vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
     else
        vacc = 1d0
     end if
     m%ppi(t) = (0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1))*vacc
  end do

  diff = 1d0
  m%iter = 0

  ! BELIEF ABOUT TRANSMISSABILITY
  ppi_belief = ppi_0

  tp = 40
  m%ns(1:tp) = m%n_bar

  ! COMPUTE MASSES UNTIL tp
  do t=1,tp
     if(t.ge.tvacc) then
        vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
     else
        vacc = 1d0
     end if

     m%ppif(t) = 1d0 - exp(-ppi_belief*m%mi(t)*m%n_bar*lf_sick*vacc)

     ! UPDATE MASSES
     m%ms(t+1) = m%ms(t) * (1d0 - m%ns(t)*m%ppif(t))
     m%mi(t+1) = m%mi(t) * (1d0 - gamma) + m%ms(t) * (m%ns(t)*m%ppif(t))
     m%mr(t+1) = m%mr(t) + m%mi(t) * gamma * (1d0 - delta)
     m%md(t+1) = m%md(t) + m%mi(t) * gamma * delta
  end do

  do while(diff.gt.erro7)
     m%iter = m%iter + 1

     ! FINAL VALUES AND INITIAL STATES
     m%vs(tt)    = m%vr
     m%ns(tt)    = m%n_bar

     ! COMPUTE OPTIMAL POLICIES AND VALUES
     do t=tt-1,tp+1,-1
        if(t.ge.tvacc) then
           vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
        else
           vacc = 1d0
        end if

        ! BELIEF
        !m%ppi(t) = m%ppi(t) * vacc
        ppit0    = m%ppi(t) * vacc

        ! SOLVING LABOR SUPPLY
        v1   = m%vs(t+1)      
        aux  = beta*ppit0*(v1-m%vi)
        if(aux.gt.erro6)then        
           n0   = ((1d0+lambda_p+aux) - sqrt((1d0+lambda_p+aux)**2d0 - 4d0*aux)) / (2d0*aux)
           v0   = utility(n0) + beta * ( (1d0-n0*ppit0)*v1 + (n0*ppit0)*m%vi )
           if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
        else
           n0   = m%n_bar
           v0   = utility(n0) + beta* ( (1d0-n0*ppit0)*v1 + beta*(n0*ppit0)*m%vi )
           if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
        end if

        ! UPDATE VALUES AND LABOR SUPPLY
        m%vs(t) = v0
        m%ns(t) = n0
     end do

     ! COMPUTE TRANSITIONS
     do t=tp,tt
        if(t.ge.tvacc) then
           vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
        else
           vacc = 1d0
        end if

        m%ppif(t) = 1d0 - exp(-ppi_belief*m%mi(t)*m%n_bar*lf_sick*vacc)

        ! UPDATE MASSES
        m%ms(t+1) = m%ms(t) * (1d0 - m%ns(t)*m%ppif(t))
        m%mi(t+1) = m%mi(t) * (1d0 - gamma) + m%ms(t) * (m%ns(t)*m%ppif(t))
        m%mr(t+1) = m%mr(t) + m%mi(t) * gamma * (1d0 - delta)
        m%md(t+1) = m%md(t) + m%mi(t) * gamma * delta
     end do

     ! criterium
     diff = maxval(abs(m%ppi-m%ppif))
     if(mod(m%iter,20).eq.0) write(*,'(i5,f15.10)'),m%iter,diff
     m%ppi = m%ppi + 0.1d0*(m%ppif-m%ppi)
  end do
  print*,""

  do t=1,50
     write(*,'(12f15.4)')dble(t),m%ppi(t),m%ns(t),m%mi(t)*100d0,m%ms(t)*100d0,m%md(t)*100d0
  end do
  print*,""

  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','min hrs inf','R0','mass ms','mass mi','welfare'
  write(*,'(12f15.4)')maxval(m%mi)*100d0,dble(maxloc(m%mi)),dble(maxloc(m%md(2:tt)-m%md(1:tt-1))), &
       maxval(m%md(2:tt)-m%md(1:tt-1))*pop,m%md(120)*pop,&
       minval(m%ns)*100d0, &
       ((m%ms(1)-m%ms(2))/m%mi(1))/gamma, &
       m%ms(1),m%mi(1),(m%ms(1)*m%vs(1)+m%mi(1)*m%vi)/m%vr

  t2 = omp_get_wtime()
  print*,''
  print*,'Program execution time: ',t2-t1

  deallocate( &
       m%vs,m%ns, &
       m%ppi,m%ppif, &
       m%ms,m%mi,m%mr,m%md)

end program main
