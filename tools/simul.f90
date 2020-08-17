program simulation
  use prec; use parameters; use uteis; use process_alt; use globals
  implicit none

  real(dp) &
       k(nk),o(no),prbo(no,no),e(ne),prbe(ne,ne), &
       v(nk,no,ne),vg(nk,no,ne),expec(nk,no)
  integer kp(nk,no,ne),kfact

  real(dp) tseries(n_simul,t_simul,n_save),maxk,mink,profits
  real(dp) fcumul(no),unif_rand1,unif_rand2,rhs,lhs,le,rhs2,lhs2
  integer t,n,counter,countku,countkl,num_obs, &
       ik1,ik2,io1,io2,ie1,ie2,i
  integer s1,s3
  integer, dimension(:), allocatable:: s2

  ! interpolation
  real(dp) &
       ttk(nk+kk),  tto(no+ko), &
       wwork(nk*no + max(2*kk*(nk+1),2*ko*(no+1))), &
       bbcoef(nk,no) , &
       wwork2s(3*max(kk,ko)+ko)
  real(dp) db2val,zeroin
  integer iflag

  ! Call policy functions and initiate seeds
  open(1,file='functions/kp.txt', position="rewind");read(1,*) kp;close(1)
  open(1,file='functions/kfact.txt', position="rewind");read(1,*) kfact;close(1)
  open(1,file='functions/v.txt', position="rewind");read(1,*) v;close(1)
  open(1,file='functions/expec.txt', position="rewind");read(1,*) expec;close(1)
  open(1,file='functions/k.txt', position="rewind");read(1,*) k;close(1)
  open(1,file='functions/o.txt', position="rewind");read(1,*) o;close(1)
  open(1,file='functions/e.txt', position="rewind");read(1,*) e;close(1)
  open(1,file='functions/prbo.txt', position="rewind");read(1,*) prbo;close(1)
  open(1,file='functions/prbe.txt', position="rewind");read(1,*) prbe;close(1)

  iflag = 0; call db2ink(k,nk,o,no,expec,nk,kk,ko,ttk,tto,bbcoef,wwork,iflag)

  ! call random_seed()
  s3 = 244+162; call random_seed(size=s1); allocate(s2(s1)); s2 = s3
  call random_seed(put=s2)
  deallocate(s2)

  countku =0; countkl=0; counter=1
  open(1,file='data/export.txt')
  maxk = 0d0
  mink = kmax
  num_obs = 0; n = 1

  do n=1,n_simul; t=1
     call random_number(unif_rand1);call random_number(unif_rand2)
     ik1 = nk/2+1; io1=no/2 + 1; ie1=2

     ik2 = kp(ik1,io1,ie1)
     profits = profit(k(ik1),o(io1)) - invadj(pk,c2,k(ik1),k(ik2))*exp(e(ie1))
     fcumul(1) = prbo(io1,1)
     do i=2,no; fcumul(i) = fcumul(i-1) + prbo(io1,i)
     end do; call position(unif_rand1,fcumul,no,i,io2)
     fcumul(1) = prbe(ie1,1)
     do i=2,ne; fcumul(i) = fcumul(i-1) + prbe(ie1,i)
     end do; call position(unif_rand2,fcumul,ne,i,ie2)

     tseries(n,t,1:9) = (/real(n,dp),real(t,dp),o(io1),e(ie1),profits, &
          k(ik1),k(ik2),k(ik2),0d0/)
     if(t.gt.t_simul-t_remain) write(1,'(9(f12.6))')tseries(n,t,1:9)

     if(k(ik1).gt.maxk) maxk=k(ik1)
     if(k(ik1).lt.mink) mink=k(ik1)

     do t=2,t_simul
        call random_number(unif_rand1); call random_number(unif_rand2)
        ik1=ik2; io1=io2; ie1 = ie2

        ik2 = kp(ik1,io1,ie1)
        profits = profit(k(ik1),o(io1)) - invadj(pk,c2,k(ik1),k(ik2))*exp(e(ie1))
        fcumul(1) = prbo(io1,1)
        do i=2,no; fcumul(i) = fcumul(i-1) + prbo(io1,i)
        end do; call position(unif_rand1,fcumul,no,i,io2)
        fcumul(1) = prbe(ie1,1)
        do i=2,ne; fcumul(i) = fcumul(i-1) + prbe(ie1,i)
        end do; call position(unif_rand2,fcumul,ne,i,ie2)

        le = 0d0; fcumul(1) = 0d0
        if(t.eq.t_simul)then
           fcumul(1) = zeroin(k(1),k(nk),qfunc,10d-8)
           fcumul(1) = max(fcumul(1),(1d0-delta)*k(ik1))
           rhs = beta*db2val(k(ik2),o(io1),1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s)

           lhs = pk+c2*(k(ik2)-k(ik1)*(1d0-delta))/k(ik1)
           !le = log(rhs)-log(lhs)
           rhs2 = beta*db2val(fcumul(1),o(io1),1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s)

           lhs2 = pk+c2*(fcumul(1)-k(ik1)*(1d0-delta))/k(ik1)
           le  = log(rhs2)-log(lhs2)
           ! write(*,'(9(f16.6))')fcumul(1),k(ik2), &
           !      fcumul(1)-(1d0-delta)*k(ik1), &
           !      k(ik2)-(1d0-delta)*k(ik1),fcumul(1)-k(ik2), &
           !      e(ie1),log(rhs)-log(lhs), &
           !      log(rhs2)-log(lhs2)
           ! if(n.ge.265) stop

        end if

        tseries(n,t,1:9) = (/real(n,dp),real(t,dp),o(io1),e(ie1),profits, &
             k(ik1),k(ik2),fcumul(1),le/)
        if(t.eq.t_simul) write(1,'(9(f12.6))')tseries(n,t,1:9)

        if(k(ik1).gt.maxk) maxk=k(ik1)
        if(k(ik1).lt.mink) mink=k(ik1)
        counter = counter + 1
        if(ik2.ge.nk) countku=countku+1
        if(ik2.le.1)  countkl=countkl+1
     end do
  end do

  close(1)
  open(1,file='data/tseries.txt',position='rewind');write(1,*)tseries;close(1)

  print*,"-->N times at upper bound:",countku,real(countku)/real(counter)
  print*,"-->N times at lower bound:",countkl,real(countkl)/real(counter)
  print*,"-->Max k and min k       :",maxk,mink

contains
  real(dp) function qfunc(xx)
    implicit none
    real(dp) xx,rhs,lhs,db2val

    rhs = beta*db2val(xx,o(io1),1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s)

    lhs = (pk+c2*(xx-k(ik1)*(1d0-delta))/k(ik1))*exp(e(ie1))

    qfunc = rhs - lhs
  end function qfunc



end program simulation
  
