program main
  use prec; use parameters; use omp_lib; use globals; use values
  implicit none

  type(model) m
  integer t,i
  real(dp) diff,n0,n1,aux,v1,v0,ppit0,ppi_hat,ndeaths
  real(dp) diff_deaths(tt),agg_hours(tt)

  print*,""
  print*,"Initiate program ..."
  print*,""

  t1 = omp_get_wtime()

  allocate( &
       m%vs(tt),m%ns(tt), &
       m%ppi(tt),m%ppif(tt),m%ppi_tilde(tt),&
       m%ms(tt+1),m%mi(tt+1),m%mr(tt+1),m%mc(tt+1),m%md(tt+1),m%nc(tt+1))

  ! Compute invariable variables and values
  m%n_bar = 1d0/(1d0+lambda_p)
  m%n_bari= 1d0/(1d0+lambda_pp)
  m%vc    = utility(m%n_bar) / (1d0 - beta)
  m%vr    = (0d0 + beta*theta*(1d0-delta)*m%vc) / (1d0 - beta*(1d0-theta))
  m%vi    = (utilityi(m%n_bari) + beta*gamma*m%vr) / (1d0 - beta*(1d0-gamma))
  
  ! Initial guess
  do t=1,tt
     m%ppi(t) = 0.50d0 + dble(t-1)*(0d0 - 0.50d0)/dble(tt-1)
  end do

  do i=1,4
     ! Iterations
     select case (i)
     case(1)
        tlag = tl1
        print*,'with lag:',tlag
        open(1,file='data/simul1.txt',position="rewind")
     case(2)
        tlag = tl2
        print*,'with lag:',tlag
        open(1,file='data/simul2.txt',position="rewind")
     case(3)
        tlag = tl3
        print*,'with lag:',tlag
        open(1,file='data/simul3.txt',position="rewind")
     case(4)
        tlag = tl4
        print*,'with lag:',tlag
        open(1,file='data/simul4.txt',position="rewind")
     end select

     diff = 1d0
     m%iter = 0
     do while(diff.gt.erro8)
        m%iter = m%iter + 1

        ! final values and initial states
        m%vs(tt) = m%vc
        m%ns(tt) = m%n_bar
        m%ppi_tilde = 0d0

        m%ms(1) = 1d0 - initinf
        m%mi(1) = 1d0 - m%ms(1)
        m%mr(1) = 0d0
        m%mc(1) = 0d0
        m%md(1) = 0d0
        m%nc(1) = 0d0

        ! compute optimal policies and values
        do t=tt-1,1,-1
           if(t.gt.tlag)then
              ppit0 = m%ppi(t-tlag)
           else
              ppit0 = 0d0
           end if
           v1   = m%vs(t+1)           
           aux  = beta*ppit0*(v1-m%vi)
           if(aux.gt.erro6)then        
              n0   = ((1d0+lambda_p+aux) - sqrt((1d0+lambda_p+aux)**2d0 - 4d0*aux)) / (2d0*aux)
              v0   = utility(n0) + beta*(1d0-n0*ppit0)*v1 + beta*(n0*ppit0)*m%vi
              if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
           else
              n0   = m%n_bar
              v0   = utility(n0) + beta*(1d0-n0*ppit0)*v1 + beta*(n0*ppit0)*m%vi
              if(v0.le.0d0) stop 'FATAL ERROR: negative value susceptible'
           end if
           m%vs(t) = v0
           m%ns(t) = n0
           m%ppi_tilde(t) = ppit0
        end do

        ! compute transition
        do t=1,tt
           ppi_hat   = ppi_0*m%mi(t)*m%n_bari
           m%ppif(t) = 1d0 - exp(-ppi_hat)

           m%ms(t+1) = m%ms(t)*(1d0-m%ns(t)*m%ppif(t))
           m%mi(t+1) = m%ms(t)*(m%ns(t)*m%ppif(t)) + m%mi(t)*(1d0-gamma)      
           m%mr(t+1) = m%mi(t)*gamma + m%mr(t)*(1d0-theta)
           m%mc(t+1) = m%mr(t)*theta*(1d0-delta) + m%mc(t)
           m%md(t+1) = m%md(t) + theta*delta*m%mr(t)

           m%nc(t+1) = m%ms(t) - m%ms(t+1)
        end do

        ! criterium
        diff = maxval(abs(m%ppi-m%ppif))
        if(mod(m%iter,200).eq.0) write(*,'(i5,f15.10)'),m%iter,diff
        m%ppi = m%ppi + 0.05d0*(m%ppif-m%ppi)
     end do
     print*,""

     do t=1,tt
        if(t.eq.1) then
           ndeaths = 0d0
           diff_deaths(t) = 0d0
        else
           ndeaths = (m%md(t)-m%md(t-1))*popfact
           diff_deaths(t) = m%md(t)-m%md(t-1)
        end if

        agg_hours(t) = m%ms(t)*m%ns(t)+m%mi(t)*m%n_bari+m%mc(t)*m%n_bar
        
        write(1,'(i15,14f15.6)') t, &
             m%ppi(t),m%ppi_tilde(t),m%ns(t), &
             m%nc(t),m%ms(t),m%mi(t),m%mr(t),m%md(t), &
             m%ms(t)+m%mi(t)+m%mr(t)+m%mc(t)+m%md(t), &
             m%ms(t)*m%ns(t)+m%mi(t)*m%n_bari+m%mc(t)*m%n_bar, &
             ndeaths
     end do
     close(1)

     write(*,'(12a15)')'pk inf','days pk','days pk dd','max dly dd','total dd(120)','agg h at thgh','min hrs inf','R0','mass ms','mass mi','welfare'
     write(*,'(12f15.4)')maxval(m%mi)*100d0,dble(maxloc(m%mi)),dble(maxloc(diff_deaths)), &
          maxval(diff_deaths)*popfact,m%md(120)*popfact,&
          minval(agg_hours),minval(m%ns)*100d0,(m%nc(2)/m%mi(1))/gamma, &
          m%ms(1),m%mi(1),(m%ms(1)*m%vs(1)+m%mi(1)*m%vi)*(1d0-beta)
     print*,''
     write(*,'(a15,f15.7)')'value',m%vs(1)
     print*,''
     print*,''
  end do

  t2 = omp_get_wtime()
  print*,''
  print*,'Program execution time: ',t2-t1
  print*,""


  deallocate( &
       m%vs,m%ns, &
       m%ppi,m%ppif,m%ppi_tilde,&
       m%ms,m%mi,m%mr,m%mc,m%md,m%nc)

end program main
