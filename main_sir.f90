program main
  use prec; use parameters; use omp_lib; use globals; use values; use equilibrium
  implicit none

  type(model) m
  integer t,i,tb,tu,istate
  real(dp) vacc,ppi_belief,ppi_curr,ppi_i,ppi_f,delay,mdr,mdr_1,aux, coeff
  real(dp) vvacc(tt),s0,i0,c0
  real(dp) zero,machep,ttol,aaa,bbb
  real(dp) pprob(61,4)

  integer s1,iz0,iz1,n
  integer, dimension(:),allocatable::s2

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()
  coeff = 0.04d0
  
  allocate(m%vs(tt),m%ppi_0(tt),m%ppi_b(tt),m%ppi(tt),m%ppie(tt),m%mdr(tt+1),m%mdrr(tt+1),m%mdf(tt+1),m%delay(tt+1),m%mrandom(tt+1))
  allocate(m%ms( tt+1),m%mi( tt+1),m%mc( tt+1),m%mr( tt+1),m%md( tt+1))
  allocate(m%mse(tt+1),m%mie(tt+1),m%mce(tt+1),m%mre(tt+1),m%mde(tt+1))
  allocate(m%msb(tt+1),m%mib(tt+1),m%mcb(tt+1),m%mrb(tt+1),m%mdb(tt+1))
  allocate(m%ns(tt),m%nse(tt))
  
  call random_seed(size=s1); allocate(s2(s1));
  do i=1,s1
     s2(i) = s3 + i
  end do
  call random_seed(put=s2)
  call random_number(m%mrandom)

  ! COLLECT HISTOGRAM PROBABILITIES
  open(1,file="Delays_mexico_states.csv",position="rewind")
  do i=1,61
     read(1,*)pprob(i,:)
  end do
  close(1)
  do i=1,4
     pprob(:,i) = pprob(:,i) / sum(pprob(:,i))
  end do

  ! SIMULATION WITH SIR MODEL

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
  istate = 4
  
  !write(*,'(a10,5a15)')'time','nse','mde','mdr'
  do t=1,tt

     if(t.lt.tvacc) then
        m%mse(t+1) = m%mse(t) - (gamma*r0)*m%mse(t)*m%mie(t)
     else
        m%mse(t+1) = m%mse(t)
     end if
     m%mie(t+1) = m%mie(t)*(1d0-gamma) + (gamma*r0)*m%mse(t)*m%mie(t)
     m%mce(t+1) = m%mce(t)*(1d0-theta) + gamma*m%mie(t)
     m%mre(t+1) = m%mre(t) + (1d0-delta)*theta*m%mce(t)
     m%mde(t+1) = m%mde(t) + delta*theta*m%mce(t)

     call deaths_reported(m%mrandom(t),61,pprob(:,istate),t,m%mde(1:t),mdr_1,m%mdr(t),m%delay(t))

     if(t.le.3)then
        m%mdrr(t) = m%mdr(t)
     end if
     
     if(t.gt.3)then
        m%mdrr(t-3:t) = m%mdr( t-3:t)*((1d0-coeff)+m%mrandom(t-3)*(2d0*coeff))
        aux = 1d0
     else
        aux= 0d0
     end if
     if(t.gt.1) then
        write(1,'(i15,20f20.10)')t,m%mie(t)*pop,m%mse(t),m%mde(t)*pop,pop*(m%mde(t)-m%mde(t-1)),0d0,0d0, &
             aux,m%mie(t),0d0,0d0,m%mdrr(t)*pop,pop*(m%mdrr(t)-m%mdrr(t-1)),0d0
     end if
     mdr_1 = m%mdr(t)

     !print*,t,m%mse(t+1),m%mse(t),gamma,r0,m%mse(t)*m%mie(t),m%mie(t)
  end do
  close(1)

  print*,"Delays model:"
  print*,""
  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(500)','min hrs inf','min hrs agg','R0','mass ms','mass mi'
  write(*,'(12f15.4)')maxval(m%mie)*100d0,dble(maxloc(m%mie)),dble(maxloc(m%mde(2:tt)-m%mde(1:tt-1))), &
       maxval(m%mde(2:tt)-m%mde(1:tt-1))*pop,m%mde(120)*pop,m%mde(500)*pop,&
       0.0d0,0.0d0, &
       ((m%mse(1)-m%mse(2))/m%mie(1))/gamma, &
       m%mse(1),m%mie(1)
  write(*,'(2a15,4f15.4)')'','', &
       dble(maxloc(m%mdr(2:tt)-m%mdr(1:tt-1))), &
       maxval(m%mdr(2:tt)-m%mdr(1:tt-1))*pop,m%mdr(120)*pop,m%mdr(500)*pop

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
