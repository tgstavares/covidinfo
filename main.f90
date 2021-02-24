program main
  use prec; use parameters; use omp_lib; use globals; use values; use equilibrium
  implicit none

  type(model) m
  integer t,i,tb,tu
  real(dp) vacc,ppi_belief,delay,mdr,mdr_1,  aux
  real(dp) zero,machep,ttol,aaa,bbb

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()

  allocate( &
       m%vs(tt),m%ns(tt),m%nse(tt), &
       m%ppi(tt), &
       m%ms(tt+1),m%mi(tt+1),m%mr(tt+1),m%md(tt+1),m%mdr(tt+1),m%mdf(tt+1), &
       m%delay(tt+1))

  ! INIT COND

  m%ms(1) = 1d0 - initinf
  m%mi(1) = 1d0 - m%ms(1)
  m%mr(1) = 0d0
  m%md(1) = 0d0

  ! INITIAL GUESS
  do t=1,tt
     if(t.ge.tvacc) then
        vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
     else
        vacc = 1d0
     end if
     m%ppi(t) = (0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1))*vacc
  end do
  
  ! BASELINE
  print*,"No friction model:"
  call get_equil(1,ppi_0,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mr,m%md,showoutput='n')
  print*,""
  m%vr = utility(n_bar) / (1d0 - beta)
  m%vi = (utility(n_bar*lf_sick) + beta*gamma*(1d0-delta)*m%vr) / (1d0 - beta*(1d0-gamma))
  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(500)','min hrs inf','R0','mass ms','mass mi','welfare'
  write(*,'(12f15.4)')maxval(m%mi)*100d0,dble(maxloc(m%mi)),dble(maxloc(m%md(2:tt)-m%md(1:tt-1))), &
       maxval(m%md(2:tt)-m%md(1:tt-1))*pop,m%md(120)*pop,m%md(500)*pop,&
       minval(m%ns)*100d0, &
       ((m%ms(1)-m%ms(2))/m%mi(1))/gamma, &
       m%ms(1),m%mi(1),(m%ms(1)*m%vs(1)+m%mi(1)*m%vi)/m%vr
  print*,""
  open(1,file="data/Epi_nofrictions.txt",position="rewind")
  do t=2,tt
     write(1,'(i15,10f20.10)')t,m%mi(t)*pop,m%ms(t),m%md(t)*pop,pop*(m%md(t)-m%md(t-1)),m%ppi(t),m%ns(t)/n_bar
  end do
  close(1)
  
  ! INIT BELIEF ABOUT TRANSMISSABILITY
  ppi_belief = ppi_0
  call get_equil(1,ppi_belief,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mr,m%mdf,showoutput='n')
  m%nse = m%ns
  
  ! SIMULATION OF EPIDEMIC
  mdr_1 = m%md(1)

  
  open(1,file="data/Epi_delays.txt",position="rewind")
  !write(*,'(a10,3a15,3a20,2a15)')'time','nbar','nse','ppi_belief','md','mdf','mdr','delay','aux'
  do t=1,tt

     call update_masses(1,t,ppi_0,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mr(1:t+1),m%md(1:t+1))
     call deaths_reported(t,m%md(1:t),mdr_1,m%mdr(t),m%delay(t))
     
     aux = 0d0
     if(abs(m%mdr(t)-m%mdf(t))*pop.ge.0.01d0.and.t.ge.1.and.1.eq.1)then
        aux = 1d0
        if(m%mdf(t).gt.m%mdr(t))then
           aaa = min(ppi_belief+1d0,15d0)
           bbb = max(ppi_belief/15d0,0d0)
        else
           aaa = max(ppi_belief,0d0)
           bbb = min((ppi_belief+1d0)*15d0,15d0)
        end if
        if(ferror_deaths(aaa)*ferror_deaths(bbb).ge.0d0)then
           aux = 2d0
           ppi_belief = 0d0
        else
           aux = 3d0
           machep = epsilon(machep)
           ttol   = machep
           ppi_belief = zero(aaa,bbb,machep,ttol,ferror_deaths)           
        end if
        call update_masses(1,t,ppi_belief,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mr(1:t+1),m%mdf(1:t+1))
        call get_equil(t,ppi_belief,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mr,m%mdf,showoutput='n')
        m%nse(t:tt) = m%ns(t:tt)
        call update_masses(1,t,ppi_0,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mr(1:t+1),m%md(1:t+1))
     end if

     ! if(t.lt.490) then
     !    write(*,'(i10,3f15.6,3e20.7,5f15.4)')t,n_bar,m%nse(t),ppi_belief,m%md(t),m%mdf(t),m%mdr(t),m%delay(t),aux,(m%md(t)-m%md(max(t-1,1)))*pop,aaa,bbb
     ! else
     !    stop
     ! end if

     if(t.gt.1) then
        write(1,'(i15,20f20.10)')t,m%mi(t)*pop,m%ms(t),m%md(t)*pop,pop*(m%md(t)-m%md(t-1)),m%ppi(t),m%nse(t)/n_bar, &
             aux,ppi_0,ppi_belief,m%delay(t),m%mdr(t)*pop,pop*(m%mdr(t)-m%mdr(t-1))
     end if
     mdr_1 = m%mdr(t)
  end do
  !write(*,'(a10,3a15,3a20,2a15)')'time','nbar','nse','ppi_belief','md','mdf','mdr','delay','aux'  
  close(1)

  print*,"Delays model:"
  print*,""
  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(500)','min hrs inf','R0','mass ms','mass mi'
  write(*,'(12f15.4)')maxval(m%mi)*100d0,dble(maxloc(m%mi)),dble(maxloc(m%md(2:tt)-m%md(1:tt-1))), &
       maxval(m%md(2:tt)-m%md(1:tt-1))*pop,m%md(120)*pop,m%md(500)*pop,&
       minval(m%nse)*100d0, &
       ((m%ms(1)-m%ms(2))/m%mi(1))/gamma, &
       m%ms(1),m%mi(1)
  write(*,'(2a15,4f15.4)')'','', &
       dble(maxloc(m%mdr(2:tt)-m%mdr(1:tt-1))), &
       maxval(m%mdr(2:tt)-m%mdr(1:tt-1))*pop,m%mdr(120)*pop,m%mdr(500)*pop
  print*,""


  t2 = omp_get_wtime()
  print*,''
  print*,'Program execution time: ',t2-t1

  deallocate( &
       m%vs,m%ns,m%nse, &
       m%ppi, &
       m%ms,m%mi,m%mr,m%md,m%mdr,m%mdf,m%delay)

contains
  real(dp) function ferror_deaths(ppiupdate)
    implicit none
    real(dp), intent(in)::ppiupdate
    real(dp) mmd(1:tt+1)
    mmd(1) = m%md(1)
    call update_masses(1,t,ppiupdate,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mr(1:t+1),mmd(1:t+1))
    ferror_deaths = (mmd(t)-m%mdr(t))
    !print*,ppiupdate,ferror_deaths
  end function ferror_deaths
end program main
