program main
  use prec; use parameters; use omp_lib; use globals; use values; use equilibrium
  implicit none

  type(model) m
  integer t,i,tb,tu
  real(dp) vacc,ppi_belief,ppi_curr,ppi_i,ppi_f,delay,mdr,mdr_1,  aux
  real(dp) zero,machep,ttol,aaa,bbb

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()

  allocate(m%vs(tt),m%ppi_0(tt),m%ppi_b(tt),m%ppi(tt),m%ppie(tt),m%mdr(tt+1),m%mdf(tt+1),m%delay(tt+1))
  allocate(m%ms( tt+1),m%mi( tt+1),m%mc( tt+1),m%mr( tt+1),m%md( tt+1))
  allocate(m%mse(tt+1),m%mie(tt+1),m%mce(tt+1),m%mre(tt+1),m%mde(tt+1))
  allocate(m%msb(tt+1),m%mib(tt+1),m%mcb(tt+1),m%mrb(tt+1),m%mdb(tt+1))
  allocate(m%ns(tt),m%nse(tt))

  ! INIT COND

  m%ms(1) = 1d0 - initinf
  m%mi(1) = 1d0 - m%ms(1)
  m%mc(1) = 0d0
  m%mr(1) = 0d0
  m%md(1) = 0d0

  m%ppi_0 = ppi_0
  m%ppi_0(tppi_0:tppi_1) = ppi_0*sppi_0

  ! INITIAL GUESS
  do t=1,tt
     if(t.ge.tvacc) then
        vacc = (1d0-1d0/dble(t-tvacc+1)) * 0d0
     else
        vacc = 1d0
     end if
     m%ppi(t) = (0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1))*vacc
  end do

  ! SIMULATION WITH PERFECT INFO
  print*,"No friction model:"
  !call get_equil(1      ,ppi_0      ,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%md,showoutput='n')
  !call get_equil(tppi_0,ppi_0*sppi_0,m%ns,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%md,showoutput='n')
  do t=1,tt
     call get_equil(t,m%ppi_0(t),m%ns,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%md,showoutput='n')
  end do

  print*,""
  m%vr = utility(n_bar) / (1d0 - beta)
  m%vc = (utility(n_bar*lf_sick) + beta*theta*(1d0-delta)*m%vr) / (1d0 - beta*(1d0-theta))
  m%vi = (utility(n_bar*lf_sick) + beta*gamma*m%vc) / (1d0 - beta*(1d0-gamma))
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

  !ppi_i = ppi_0
  !ppi_f = ppi_0*sppi_0

  ppi_belief = m%ppi_0(1)
  call get_equil(1,ppi_belief,m%nse,m%vs,m%ppi,m%msb,m%mib,m%mcb,m%mrb,m%mdb,showoutput='n')
  mdr_1 = m%mde(1)

  open(1,file="data/Epi_delays.txt",position="rewind")

  write(*,'(a10,5a15,3a20,2a15)')'time','nbar','nse','ppi_belief','','','md','mdf','mdr','delay','aux'

  do t=1,tt
     ppi_curr = m%ppi_0(t)
     !if(t.ge.tppi_0) ppi_curr = ppi_0*sppi_0
     call update_masses(t,t,ppi_curr,m%nse,m%ppie,m%mse,m%mie,m%mce,m%mre,m%mde)
     call deaths_reported(t,m%mde(1:t),mdr_1,m%mdr(t),m%delay(t))

     !print*,t,m%mdr(t),m%mdb(t),abs(m%mdr(t)-m%mdb(t))*pop.ge.erro6

     aux = 0d0
     if(abs(m%mdr(t)-m%mdb(t))*pop.ge.erro6.and.t.ge.1.and.1.eq.1)then

        aux = 1d0
        if(m%mdb(t).gt.m%mdr(t))then
           aaa = 50d0!min(ppi_belief+1d0,55d0)
           bbb = 0d0
        else
           aaa = 0d0
           bbb = 50d0!min(ppi_belief+1d0,55d0)
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

        call update_masses(max(t-tscr,1),t,ppi_belief,m%nse,m%ppi,m%msb,m%mib,m%mcb,m%mrb,m%mdb)
        call get_equil(t,ppi_belief,m%nse,m%vs,m%ppi,m%msb,m%mib,m%mcb,m%mrb,m%mdb,showoutput='n')

        ! if(t.le.55) then
        !    print*,t,m%mdr(t),m%mdb(t),ppi_belief
        ! else
        !    stop
        !    !if(t.eq.6) stop
        ! end if

        ! print*,'---'
        ! print*,t,aaa,bbb
        ! print*,ppi_belief
        ! print*,m%mib(t-2:t)
        ! print*,m%mie(t-2:t)
        ! print*,'---'
        ! print*,m%mdb(1:t)
        ! print*,m%mde(1:t)
        ! print*,m%mdr(1:t)/m%mdb(1:t)
        ! print*,'==================='
        ! if(t.eq.6) stop

        ! if(t.lt.tppi_0)then
        !    call update_masses(1,t,ppi_belief,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),m%mdf(1:t+1))
        !    call get_equil(t,ppi_belief,m%nse,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%mdf,showoutput='n')
        !    ppi_i = ppi_belief
        ! else
        !    call update_masses(1,min(tppi_0,tt),ppi_i     ,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),m%mdf(1:t+1))
        !    call update_masses(tppi_0-tlag*2+1,t       ,ppi_belief,m%nse(1:t),m%ppi(1:t),m%ms(1:t+1),m%mi(1:t+1),m%mc(1:t+1),m%mr(1:t+1),m%mdf(1:t+1))
        !    call get_equil(t,ppi_belief,m%nse,m%vs,m%ppi,m%ms,m%mi,m%mc,m%mr,m%mdf,showoutput='n')
        !    ppi_f = ppi_belief           
        ! end if

        ! if(t.eq.203)then
        !    print*,t,ppi_belief
        !    do i = t-10,t
        !       print*,i,m%nse(i),m%ms(i),m%mse(i)
        !    end do
        !    !stop
        ! end if

        call update_masses(t,t,ppi_curr,m%nse(1:t),m%ppie(1:t),m%mse(1:t+1),m%mie(1:t+1),m%mce(1:t+1),m%mre(1:t+1),m%mde(1:t+1))
     end if

     ! if(t.lt.tppi_0+20) then
     !    if(t.ge.tppi_0-5)then
     !if(t.le.400)then
     !      write(*,'(i10,5f15.6,3e20.7,5f15.4)')t,n_bar,m%nse(t),ppi_belief,ppi_i,ppi_f,m%mde(t),m%mdb(t),m%mdr(t),m%delay(t),aux,ppi_curr,aaa,bbb
     !   end if
     !   if(t.eq.210) stop
     !else
     !   stop
     !end if

     if(t.gt.1) then
        write(1,'(i15,20f20.10)')t,m%mie(t)*pop,m%mse(t),m%mde(t)*pop,pop*(m%mde(t)-m%mde(t-1)),m%ppie(t),m%nse(t)/n_bar, &
             aux,ppi_curr,ppi_belief,m%delay(t),m%mdr(t)*pop,pop*(m%mdr(t)-m%mdr(t-1))
     end if
     mdr_1 = m%mdr(t)
  end do
  !write(*,'(a10,5a15,3a20,2a15)')'time','nbar','nse','ppi_belief','md','mdf','mdr','delay','aux'  
  close(1)

  print*,"Delays model:"
  print*,""
  write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','total dd(500)','min hrs inf','R0','mass ms','mass mi'
  write(*,'(12f15.4)')maxval(m%mie)*100d0,dble(maxloc(m%mie)),dble(maxloc(m%mde(2:tt)-m%mde(1:tt-1))), &
       maxval(m%mde(2:tt)-m%mde(1:tt-1))*pop,m%mde(120)*pop,m%mde(500)*pop,&
       minval(m%nse)*100d0, &
       ((m%mse(1)-m%mse(2))/m%mie(1))/gamma, &
       m%mse(1),m%mie(1)
  write(*,'(2a15,4f15.4)')'','', &
       dble(maxloc(m%mdr(2:tt)-m%mdr(1:tt-1))), &
       maxval(m%mdr(2:tt)-m%mdr(1:tt-1))*pop,m%mdr(120)*pop,m%mdr(500)*pop
  print*,""

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
