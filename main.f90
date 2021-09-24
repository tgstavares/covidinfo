program main
  use prec; use parameters; use omp_lib; use globals; use values; use equilibrium
  implicit none

  type(model) m
  integer t,i,tb,tu,istate,nnn
  real(dp) vacc,ppi_belief,ppi_curr,ppi_i,ppi_f,delay,mdr,mdr_1,aux
  real(dp) vvacc(tt),s0,i0,c0,r0
  real(dp) zero,machep,ttol,aaa,bbb
  real(dp) pprob(61,4)

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()

  allocate(m%vs(tt),m%ppi_0(tt),m%ppi_b(tt),m%ppi(tt),m%ppie(tt),m%mdr(tt+1),m%mdf(tt+1),m%delay(tt+1))
  allocate(m%ms( tt+1),m%mi( tt+1),m%mc( tt+1),m%mr( tt+1),m%md( tt+1))
  allocate(m%mse(tt+1),m%mie(tt+1),m%mce(tt+1),m%mre(tt+1),m%mde(tt+1))
  allocate(m%msb(tt+1),m%mib(tt+1),m%mcb(tt+1),m%mrb(tt+1),m%mdb(tt+1))
  allocate(m%ns(tt),m%nse(tt))

  nnn = 500
  ! COLLECT HISTOGRAM PROBABILITIES
  open(1,file="Delays_mexico_states.csv",position="rewind")
  do i=1,61
     read(1,*)pprob(i,:)
  end do
  close(1)
  do i=1,4
     pprob(:,i) = pprob(:,i) / sum(pprob(:,i))
  end do
  
  ! INIT COND

  m%ms(1) = 1d0 - initinf
  m%mi(1) = 1d0 - m%ms(1)
  m%mc(1) = 0d0
  m%mr(1) = 0d0
  m%md(1) = 0d0

  m%ppi_0 = ppi_0

  ! INITIAL GUESS
  do t=1,tt
     if(t.ge.tvacc) then
        vacc     = (1d0-1d0/dble(t-tvacc+1)) * 0d0
        vvacc(t) = (1d0-1d0/dble(t-tvacc+1)) * 0d0
     else
        vacc     = 1d0
        vvacc(t) = 1d0
     end if
     m%ppi(t) = (0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1))*vacc
  end do

  ! SIMULATION WITH PERFECT INFO
  print*,"No friction model:"
  call get_equil(1,ppi_0,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%md,showoutput='n')
  call get_equil(tppi_0,ppi_0*sppi_0,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%md,showoutput='n')
  print*,""
  m%vr = utility(n_bar) / (1d0 - beta)
  m%vc = (utility(n_bar*lf_sick) + beta*theta*(1d0-delta)*m%vr) / (1d0 - beta*(1d0-theta))
  m%vi = (utility(n_bar*lf_sick) + beta*gamma*m%vc) / (1d0 - beta*(1d0-gamma))

  m%nagg = m%ns(1:t)*m%ms(1:t) + n_bar*lf_sick*(m%mi(1:t) + m%mc(1:t)) + n_bar*m%mr(1:t)

  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(nnn)','min hrs inf','min hrs agg','R0','mass ms','mass mi','welfare'
  write(*,'(12f15.4)')maxval(m%mi)*100d0,dble(maxloc(m%mi)),dble(maxloc(m%md(2:tt)-m%md(1:tt-1))), &
       maxval(m%md(2:tt)-m%md(1:tt-1))*pop,m%md(120)*pop,m%md(nnn)*pop,&
       minval(m%ns)*100d0,minval(m%nagg(2:tt))*100d0, &
       ((m%ms(1)-m%ms(2))/m%mi(1))/gamma, &
       m%ms(1),m%mi(1),(m%ms(1)*m%vs(1)+m%mi(1)*m%vi)/m%vr
  print*,""
  open(1,file="data/Epi_nofrictions.txt",position="rewind")
  do t=2,tt
     write(1,'(i15,10f20.10)')t,m%mi(t)*pop,m%ms(t),m%md(t)*pop,pop*(m%md(t)-m%md(t-1)),m%ppi(t),m%ns(t)/n_bar,m%nagg(t)/n_bar
  end do
  close(1)

  ! SIMULATION WITH DELAYS

  m%mse(1) = 1d0 - initinf
  m%mie(1) = 1d0 - m%mse(1)
  m%mce(1) = 0d0
  m%mre(1) = 0d0
  m%mde(1) = 0d0

  m%msb(1) = 1d0 - initinf
  m%mib(1) = 1d0 - m%msb(1)
  m%mcb(1) = 0d0
  m%mrb(1) = 0d0
  m%mdb(1) = 0d0

  ppi_belief = m%ppi_0(1)
  call get_equil(1,ppi_belief,m%nse,m%vs,m%ppi,m%msb,m%mib,m%mcb,m%mrb,m%mdb,showoutput='n')
  mdr_1 = m%mde(1)

  open(1,file="data/Epi_delays.txt",position="rewind")
  istate = 1
  
  !write(*,'(a10,5a15)')'time','nse','mde','mdr'
  do t=1,tt
     if(t.lt.tppi_0)then
        ppi_curr = ppi_0
     else
        ppi_curr = ppi_0*sppi_0
     end if
     ppi_belief = ppi_curr
     call update_masses(t,t,ppi_curr,m%nse,m%ppie,m%mse,m%mie,m%mce,m%mre,m%mde)
     call deaths_reported(61,pprob(:,istate),t,m%mde(1:t),mdr_1,m%mdr(t),m%delay(t))
     !if(t.eq.20) stop
     if(t.gt.3)then
        call recov_masses(t-3,t,m%ppi_0(t-3:t),vvacc(t-3:t),m%mdr(t-3:t),m%nse(t-3:t-1),s0,i0,c0,r0)
        m%msb(t) = s0
        m%mib(t) = i0
        m%mcb(t) = c0
        m%mrb(t) = r0
        m%mdb(t) = m%mdr(t)
        !print*,t

        call get_equil(t,ppi_curr,m%nse,m%vs,m%ppi,m%msb,m%mib,m%mcb,m%mrb,m%mdb,showoutput='n')
        !call get_equil(t,ppi_curr,m%nse,m%vs,m%ppi,m%mse,m%mie,m%mce,m%mre,m%mde,showoutput='n')

        aux = 1d0
     else
        s0 = m%msb(t)
        i0 = m%mib(t)
        c0 = m%mcb(t)
        r0 = m%mrb(t)
        aux= 0d0
     end if

     ! if(t.le.145)then
     !    write(*,'(i10,10f15.6)')t,m%nse(t)/n_bar,m%mde(t)*pop,m%mdr(t)*pop,m%mse(t),s0,m%mie(t)*pop,i0*pop,m%mre(t)/m%mde(t),r0/m%mdr(t)
     ! else
     !    stop
     ! end if

     m%nagg(t) = m%nse(t)*m%mse(t) + n_bar*lf_sick*(m%mie(t) + m%mce(t)) + n_bar*m%mre(t)
     
     if(t.gt.1) then
        write(1,'(i15,20f20.10)')t,m%mie(t)*pop,m%mse(t),m%mde(t)*pop,pop*(m%mde(t)-m%mde(t-1)),m%ppie(t),m%nse(t)/n_bar, &
             aux,m%mie(t),m%mib(t),m%delay(t),m%mdr(t)*pop,pop*(m%mdr(t)-m%mdr(t-1)),m%nagg(t)/n_bar
     end if
     mdr_1 = m%mdr(t)
  end do
  close(1)

  print*,"Delays model:"
  print*,""
  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(nnn)','min hrs inf','min hrs agg','R0','mass ms','mass mi'
  write(*,'(12f15.4)')maxval(m%mie)*100d0,dble(maxloc(m%mie)),dble(maxloc(m%mde(2:tt)-m%mde(1:tt-1))), &
       maxval(m%mde(2:tt)-m%mde(1:tt-1))*pop,m%mde(120)*pop,m%mde(nnn)*pop,&
       minval(m%nse)*100d0,minval(m%nagg(2:tt))*100d0, &
       ((m%mse(1)-m%mse(2))/m%mie(1))/gamma, &
       m%mse(1),m%mie(1)
  write(*,'(2a15,4f15.4)')'','', &
       dble(maxloc(m%mdr(2:tt)-m%mdr(1:tt-1))), &
       maxval(m%mdr(2:tt)-m%mdr(1:tt-1))*pop,m%mdr(120)*pop,m%mdr(nnn)*pop

  t2 = omp_get_wtime()
  print*,''
  print*,'Program execution time: ',t2-t1

  deallocate(m%vs,m%ppi_0,m%ppi_b,m%ppi,m%ppie,m%mdr,m%mdf,m%delay)
  deallocate(m%ms,m%mi,m%mc,m%mr,m%md)
  deallocate(m%mse,m%mie,m%mce,m%mre,m%mde)
  deallocate(m%msb,m%mib,m%mcb,m%mrb,m%mdb)
  deallocate(m%ns,m%nse)

contains
  real(dp) function ferror_deaths(ppiupdate)
    implicit none
    real(dp), intent(in)::ppiupdate
    real(dp) mmd(1:tt+1)
    mmd(1) = m%md(1)

    call update_masses(max(t-tscr,1),t,ppiupdate,m%nse,m%ppi,m%msb,m%mib,m%mcb,m%mrb,mmd)

    ! if(t.lt.tppi_0)then
    !    call update_masses(1,t,ppiupdate,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),mmd(1:t+1))
    ! else
    !    call update_masses(1,min(tppi_0,tt),ppi_i    ,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),mmd(1:t+1))
    !    call update_masses(tppi_0-tlag*2+1,t       ,ppiupdate,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),mmd(1:t+1))
    ! end if

    ferror_deaths = (mmd(t)-m%mdr(t))
    !print*,ppiupdate,ferror_deaths,mmd(t),m%mdr(t),t
  end function ferror_deaths
end program main
