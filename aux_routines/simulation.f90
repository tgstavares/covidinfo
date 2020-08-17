module simulation
  use prec; use parameters; use globals
contains
  
  subroutine get_simulshocks(nsimul,tsimul,nx,no,na,o,a,prbx,prbo,prba,ixpath,iopath,iapath,randomseed,uncshocks)
    implicit none
    integer, intent(in)::nsimul,tsimul,nx,no,na
    real(dp), intent(in)::o(no),a(na),prbx(nx,nx),prbo(no,no,nx),prba(na,na)
    character(len=1),intent(in), optional::randomseed,uncshocks
    integer, intent(out)::ixpath(nsimul,tsimul),iopath(nsimul,tsimul),iapath(nsimul,tsimul)
    integer uncflag,flag,s1,s3,io0,io1,t,n,ix0,ix1,ia0,ia1
    integer, dimension(:),allocatable::s2
    real(dp) shockso(nsimul,tsimul),shocksx(tsimul),shocksa(tsimul),fcumulo(no),fcumulx(nx),fcumula(na), &
         tapath(tsimul),txpath(tsimul)
    integer i

    flag = 1
    if(present(randomseed))then
       if(randomseed(1:1).eq.'y'.or.randomseed(1:1).eq.'Y') flag = 0
    end if
    uncflag = 1
    if(present(uncshocks))then
       if(uncshocks(1:1).eq.'n'.or.uncshocks(1:1).eq.'N') uncflag = 0
    end if

    s3 = 12503
    if(flag.eq.0) call system_clock(COUNT=s3)
    call random_seed(size=s1); allocate(s2(s1));
    do i=1,s1
       s2(i) = s3 + i
    end do
    call random_seed(put=s2)
    call random_number(shockso)
    call random_number(shocksx)
    call random_number(shocksa)

    ix0 = 1
    ia0 = na/2 + 1
    txpath(1) = ix0
    tapath(1) = ia0
    do t=2,tsimul
       if(uncflag.eq.1)then
          fcumulx(1) = prbx(ix0,1)
          if(shocksx(t).le.fcumulx(1))then
             ix1 = 1
          else
             do i=2,nx
                fcumulx(i) = fcumulx(i-1) + prbx(ix0,i)
                if(fcumulx(i).gt.shocksx(t))then
                   ix1 = i
                   exit
                end if
             end do
          end if
          txpath(t) = ix1
       else
          ix1 = 1
          txpath(t) = ix1
       end if
       ix0 = ix1

       fcumula(1) = prba(ia0,1)
       if(shocksa(t).le.fcumula(1))then
          ia1 = 1
       else
          do i=2,na
             fcumula(i) = fcumula(i-1) + prba(ia0,i)
             if(fcumula(i).gt.shocksa(t))then
                ia1 = i
                exit
             end if
          end do
       end if
       tapath(t) = ia1
       ia0 = ia1       
    end do

    do n=1,nsimul
       io0 = no/2 + 1
       iopath(n,1) = io0
       ixpath(n,1) = txpath(1)
       iapath(n,1) = tapath(1)
       do t=2,tsimul
          ix0 = txpath(t-1)
          ia0 = tapath(t)

          fcumulo(1) = prbo(io0,1,ix0)
          if(shockso(n,t).le.fcumulo(1))then
             io1 = 1
          else
             do i=2,no
                fcumulo(i) = fcumulo(i-1) + prbo(io0,i,ix0)
                if(fcumulo(i).gt.shockso(n,t))then
                   io1 = i
                   exit
                end if
             end do
          end if
          iopath(n,t) = io1
          io0 = io1

          ixpath(n,t) = txpath(t)
          iapath(n,t) = tapath(t)
       end do
    end do

  end subroutine get_simulshocks

  subroutine get_simulshocksmarkov(nsimul,tsimul,nx,no,o,prbx,prbo,ixpath,iopath,randomseed)
    implicit none
    integer, intent(in)::nsimul,tsimul,nx,no
    real(dp), intent(in)::o(no),prbx(nx,nx),prbo(no,no,nx)
    character(len=1),intent(in), optional::randomseed
    integer, intent(out)::ixpath(nsimul,tsimul),iopath(nsimul,tsimul)
    integer flag,s1,s3,io0,io1,t,n,ix0,ix1
    integer, dimension(:),allocatable::s2
    real(dp) shockso(nsimul,tsimul),shocksx(nsimul,tsimul),fcumulo(no),fcumulx(nx)
    integer i

    flag = 1
    if(present(randomseed))then
       if(randomseed(1:1).eq.'y'.or.randomseed(1:1).eq.'Y') flag = 0
    end if

    s3 = 2503
    if(flag.eq.0) call system_clock(COUNT=s3)
    call random_seed(size=s1); allocate(s2(s1));
    do i=1,s1
       s2(i) = s3 + i
    end do
    call random_seed(put=s2)
    call random_number(shockso)
    call random_number(shocksx)

    do n=1,nsimul
       ix0 = 1
       io0 = no/2 + 1
       iopath(n,1) = io0
       ixpath(n,1) = ix0
       do t=2,tsimul
          fcumulx(1) = prbx(ix0,1)
          if(shocksx(n,t).le.fcumulx(1))then
             ix1 = 1
          else
             do i=2,nx
                fcumulx(i) = fcumulx(i-1) + prbx(ix0,i)
                if(fcumulx(i).gt.shocksx(n,t))then
                   ix1 = i
                   exit
                end if
             end do
          end if
          ixpath(n,t) = 1!ix1

          fcumulo(1) = prbo(io0,1,ix1)
          if(shockso(n,t).le.fcumulo(1))then
             io1 = 1
          else
             do i=2,no
                fcumulo(i) = fcumulo(i-1) + prbo(io0,i,ix1)
                if(fcumulo(i).gt.shockso(n,t))then
                   io1 = i
                   exit
                end if
             end do
          end if
          iopath(n,t) = io1

          ix0 = ix1
          io0 = io1
       end do
    end do
  end subroutine get_simulshocksmarkov

  subroutine get_simulations(p,m,nsimul,tsimul,nvars,ixpath,izpath,iapath,initconds,data,initial_conditions)
    use omp_lib; use values
    implicit none
    type(param), intent(in):: p
    type(model), intent(in):: m
    integer, intent(in)::nsimul,tsimul,nvars,ixpath(nsimul,tsimul),izpath(nsimul,tsimul),iapath(nsimul,tsimul)
    real(dp), intent(in):: initconds(nsimul,7)
    real(dp), intent(out)::data(nsimul,tsimul,nvars)
    character(len=1),intent(in), optional::initial_conditions    
    integer flag,n,t,iz0,ia0,ik0,ib0,ik1,ib1,ix0
    real(dp) z0,z1,a0,a1,k0,b0,k1,b1,inv0,inv_1,z_1,a_1,k_1,b_1,v0,wedge0,rr

    flag = 1
    if(present(initial_conditions))then
       if(initial_conditions(1:1).eq.'y'.or.initial_conditions(1:1).eq.'Y') flag = 0
    end if
    
    data = 0d0

    !$omp parallel do private(n,ik0,ib0,k0,b0,z_1,a_1,inv_1,k_1,b_1,t,ix0,iz0,ia0,z0,a0,k1,b1,inv0,v0,wedge0,rr)
    do n=1,nsimul

       if(flag.eq.1)then
          ik0   = nk * 61/100 + 1
          ib0   = nb * 95/100 + 1
          k0    = m%k(ik0)
          b0    = m%b(ib0)
          z_1   = m%z(izpath(1,1))
          a_1   = m%a(iapath(1,1))
          inv_1 = 0d0
          k_1   = k0
          b_1   = b0
       else
          k0    = initconds(n,1)
          b0    = initconds(n,2)
          z_1   = initconds(n,3)
          a_1   = initconds(n,4)
          inv_1 = initconds(n,5)
          k_1   = initconds(n,6)
          b_1   = initconds(n,7)
          !print*,n,k0,b0,z_1,a_1,inv_1,k_1,b_1
       end if

       do t=1,tsimul
          ix0  = ixpath(n,t)
          iz0  = izpath(n,t)
          ia0  = iapath(n,t)
          z0   = m%z(iz0)
          a0   = m%a(ia0)
          call get_kp_bp(p,m,iz0,ia0,ix0,k0,b0,z0,a0,v0,k1,b1,wedge0)
          inv0 = k1 - (1d0-deltak)*k0

          data(n,t,1)  = dble(n)
          data(n,t,2)  = dble(t)
          data(n,t,3)  = k0
          data(n,t,4)  = log(z0*a0)
          data(n,t,5)  = k1
          data(n,t,6)  = inv0
          data(n,t,7)  = inv0/k0
          data(n,t,8)  = log(z_1*a_1)
          data(n,t,9)  = log(z0*a0)-log(z_1*a_1)
          data(n,t,10) = inv_1/k_1
          data(n,t,11) = dble(ix0)
          data(n,t,12) = max(b0,0d0)       
          data(n,t,13) = b1
          data(n,t,14) = b1*k1
          data(n,t,15) = profit(k0,z0,a0)
          data(n,t,16) = log(z0)
          data(n,t,17) = log(a0)
          data(n,t,18) = b0*k0
          data(n,t,19) = v0
          data(n,t,20) = wedge0

          rr           = r_p
          if(abs(inv0/k0).le.erro8)then
             data(n,t,21) = cashflowfn(p,k0,b0,z0,a0,k1,b1,rr)
          else
             data(n,t,21) = cashflowfn(p,k0,b0,z0,a0,k1,b1,rr) - nconvexk(p,k0)
          end if

          z_1   = z0
          a_1   = a0
          inv_1 = inv0
          k_1   = k0
          b_1   = b0

          !print*,n,t,ix0,z0,k0,b0,k1,b1,inv0/k0          
          k0    = k1
          b0    = b1          
       end do
    end do
    !$omp end parallel do

  end subroutine get_simulations

  subroutine get_stats(nsimul,tsimul,nvars,tdiscard,data,nstats,stats,showoutput)
    implicit none
    integer, intent(in)::nsimul,tsimul,nvars,tdiscard,nstats
    real(dp),intent(in)::data(nsimul,tsimul,nvars)
    real,intent(out)::stats(nstats)
    character(len=1), intent(in), optional::showoutput
    integer flag_showoutput
    real e_ik,m_ik,a_ik,p_ik,n_ik,ina,p_spike,n_spike,sd_o,aut_o,sd_ik,aut_ik,corr_iko,corr_ikdo,e_bp,m_bp,a_bp,e_k,sd_bp
    real corr_k1o,corr_bpo,corr_ikbp,spike
    real a_sales,a_k,a_salesk,sd_mpk,a_b,a_b1,a_i,q66_bp
    real num_loss,den_loss,loss_base,loss_alt1,loss_alt2,mean_a,mean_z,var_z,agg_profits,agg_capital,a_optim

    flag_showoutput = 1
    if(present(showoutput))then
       if(showoutput(1:1).eq.'n'.or.showoutput(1:1).eq.'No') flag_showoutput = 0
    end if

    call  mean(real(data(:,tdiscard:tsimul,7)),nsimul*(tsimul-tdiscard+1)    ,0,e_ik)    
    call sampp(real(data(:,tdiscard:tsimul,7)),nsimul*(tsimul-tdiscard+1),0.5,0,m_ik)
    a_ik = real(sum(data(:,tdiscard:tsimul,6))/sum(data(:,tdiscard:tsimul,3)))
    call  mean(real(data(:,tdiscard:tsimul,12)),nsimul*(tsimul-tdiscard+1)    ,0,e_bp)
    call sampp(real(data(:,tdiscard:tsimul,12)),nsimul*(tsimul-tdiscard+1),0.5,0,m_bp)
    a_bp = real(sum(data(:,tdiscard:tsimul,14))/sum(data(:,tdiscard:tsimul,5)))    
    p_ik    = real(count(data(:,tdiscard:tsimul,7).gt. 0.01))/real(nsimul*(tsimul-tdiscard+1))
    n_ik    = real(count(data(:,tdiscard:tsimul,7).lt.-0.01))/real(nsimul*(tsimul-tdiscard+1))
    ina     = real(count(abs(data(:,tdiscard:tsimul,7)).lt.0.01))/real(nsimul*(tsimul-tdiscard+1))
    p_spike = real(count(data(:,tdiscard:tsimul,7).gt. 0.2))/real(nsimul*(tsimul-tdiscard+1))
    n_spike = real(count(data(:,tdiscard:tsimul,7).lt.-0.2))/real(nsimul*(tsimul-tdiscard+1))
    spike   = p_spike+n_spike
    call sd(real(data(:,tdiscard:tsimul,7)),nsimul*(tsimul-tdiscard+1)    ,0,sd_ik)
    call corr(real(data(:,tdiscard:tsimul,4)),real(data(:,tdiscard:tsimul,7)),nsimul*(tsimul-tdiscard+1),0,corr_iko)
    call corr(real(data(:,tdiscard:tsimul,4)),real(log(data(:,tdiscard:tsimul,5))),nsimul*(tsimul-tdiscard+1),0,corr_k1o)
    call sd(real(data(:,tdiscard:tsimul,13)),nsimul*(tsimul-tdiscard+1)    ,0,sd_bp)
    call corr(real(data(:,tdiscard:tsimul,4)),real(data(:,tdiscard:tsimul,13)),nsimul*(tsimul-tdiscard+1),0,corr_bpo)
    call corr(real(data(:,tdiscard:tsimul,7)),real(data(:,tdiscard:tsimul,13)),nsimul*(tsimul-tdiscard+1),0,corr_ikbp)

    call sd(real(data(:,tdiscard:tsimul,4)),nsimul*(tsimul-tdiscard+1)    ,0,sd_o)
    call corr(real(data(:,tdiscard:tsimul,4)),real(data(:,tdiscard:tsimul,8)),nsimul*(tsimul-tdiscard+1),0,aut_o)
    call corr(real(data(:,tdiscard:tsimul,7)),real(data(:,tdiscard:tsimul,10)),nsimul*(tsimul-tdiscard+1),0,aut_ik)
    call corr(real(data(:,tdiscard:tsimul,9)),real(data(:,tdiscard:tsimul,7)),nsimul*(tsimul-tdiscard+1),0,corr_ikdo)

    call  mean(real(data(:,tdiscard:tsimul,3)),nsimul*(tsimul-tdiscard+1)    ,0,e_k)
    ! print*,nsimul*(tsimul-tdiscard+1)
    ! print*,real(data(:,tdiscard:tsimul,3))
    ! print*,e_k
    ! !stop

    
    a_sales  = real(sum(data(:,tdiscard:tsimul,15)))
    a_k      = real(sum(data(:,tdiscard:tsimul,3)))
    a_salesk = a_sales / a_k
    a_b      = real(sum(data(:,tdiscard:tsimul,18)))
    a_b1     = real(sum(data(:,tdiscard:tsimul,14)))
    a_i      = real(sum(data(:,tdiscard:tsimul,6)))
    call sampp(real(data(:,tdiscard:tsimul,12)),nsimul*(tsimul-tdiscard+1),0.66,0,q66_bp)
    
    call sd(real(log(data(:,tdiscard:tsimul,15))-log(data(:,tdiscard:tsimul,3))),nsimul*(tsimul-tdiscard+1)    ,0,sd_mpk)

    call mean(real(data(:,tdiscard:tsimul,17)),nsimul*(tsimul-tdiscard+1),0,mean_a)
    call mean(real(exp(data(:,tdiscard:tsimul,16)*(1d0/(1d0-alpha)))),nsimul*(tsimul-tdiscard+1),0,a_optim)    
    call mean(real(data(:,tdiscard:tsimul,15)),nsimul*(tsimul-tdiscard+1),0,agg_profits)
    call mean(real(data(:,tdiscard:tsimul,3 )),nsimul*(tsimul-tdiscard+1),0,agg_capital)
    call mean(real(exp(data(:,tdiscard:tsimul,16)*(1d0/(1d0-alpha))) * (alpha*data(:,tdiscard:tsimul,15)/data(:,tdiscard:tsimul,3))**(-alpha/(1d0-alpha))),nsimul*(tsimul-tdiscard+1),0,num_loss)
    call mean(real(exp(data(:,tdiscard:tsimul,16)*(1d0/(1d0-alpha))) * (alpha*data(:,tdiscard:tsimul,15)/data(:,tdiscard:tsimul,3))**(-1d0  /(1d0-alpha))),nsimul*(tsimul-tdiscard+1),0,den_loss)
    a_optim = exp(mean_a)*a_optim**(1d0-alpha)
    loss_base = log(exp(mean_a)*num_loss/den_loss**alpha / a_optim)
    !loss_alt1 = log(agg_profits / agg_capital**alpha / a_optim)
    loss_alt1 = log(sum(data(:,tdiscard:tsimul,15)) / sum(data(:,tdiscard:tsimul,3))**alpha / (exp(mean_a)*sum(exp(data(:,tdiscard:tsimul,16)*(1d0/(1-alpha))))**(1-alpha)) )    
    loss_alt2 = -0.5d0 * (alpha/(1d0-alpha)) * sd_mpk**2d0

    stats(1) = e_ik;	  stats(2) = m_ik;	stats(3) = a_ik
    stats(4) = e_bp;	  stats(5) = m_bp;	stats(6) = a_bp
    stats(7) = p_ik;	  stats(8) = n_ik;	stats(9) = ina
    stats(10)= p_spike;	  stats(11)= n_spike;	stats(12)= spike
    stats(13)= sd_ik;	  stats(14)= corr_iko;	stats(15)= corr_k1o
    stats(16)= sd_bp;	  stats(17)= corr_bpo;	stats(18)= corr_ikbp
    stats(19)= sd_o;	  stats(20)= aut_o;	stats(21)= aut_ik
    stats(22)= corr_ikdo; stats(23)= e_k;	stats(24)= a_sales
    stats(25)= a_salesk;  stats(26)= sd_mpk;	stats(27)= a_b
    stats(28)= a_b1;	  stats(29)= a_i;       stats(30)= q66_bp
    stats(31)= loss_base; stats(32)= loss_alt1; stats(33)= loss_alt2

    if(flag_showoutput.eq.1)then
       write(*,'(21a12)') &
            'E(ik)','med(ik)','pos(ik)','neg(ik)','inaction', &
            'pos spike','neg spike','sd(o)','aut(o)','sd(ik)', &
            'aut(ik)','corr(ik,o)','corr(ik,do)', &
            'E(bp)','med(bp)','q66(bp)','sd(bp)','E(k)'
       write(*,'(21f12.4)') &
            e_ik,m_ik,p_ik,n_ik,ina, &
            p_spike,n_spike,sd_o,aut_o,sd_ik, &
            aut_ik,corr_iko,corr_ikdo, &
            e_bp,m_bp,q66_bp,sd_bp,e_k
    end if
  end subroutine get_stats

  subroutine get_simulshocks_alt(nsimul,tsimul,nx,no,na,o,a,prbx,prbo,prba,ixpath,iopath,iapath,randomseed,uncshocks)
    implicit none
    integer, intent(in)::nsimul,tsimul,nx,no,na
    real(dp), intent(in)::o(no),a(na),prbx(nx,nx),prbo(no,no,nx),prba(na,na)
    character(len=1),intent(in), optional::randomseed,uncshocks
    integer, intent(out)::ixpath(nsimul,tsimul),iopath(nsimul,tsimul),iapath(nsimul,tsimul)
    integer uncflag,flag,s1,s3,io0,io1,t,n,ix0,ix1,ia0,ia1
    integer, dimension(:),allocatable::s2
    real(dp) shockso(nsimul,tsimul),shocksx(tsimul),shocksa(tsimul),fcumulo(no),fcumulx(nx),fcumula(na), &
         tapath(tsimul),txpath(tsimul)
    integer i

    flag = 1
    if(present(randomseed))then
       if(randomseed(1:1).eq.'y'.or.randomseed(1:1).eq.'Y') flag = 0
    end if
    uncflag = 1
    if(present(uncshocks))then
       if(uncshocks(1:1).eq.'n'.or.uncshocks(1:1).eq.'N') uncflag = 0
    end if

    s3 = 12503
    if(flag.eq.0) call system_clock(COUNT=s3)
    call random_seed(size=s1); allocate(s2(s1));
    do i=1,s1
       s2(i) = s3 + i
    end do
    call random_seed(put=s2)
    call random_number(shockso)
    call random_number(shocksx)
    call random_number(shocksa)

    ix0 = 1
    ia0 = na/2 + 1
    txpath(1) = ix0
    tapath(1) = ia0
    do t=2,tsimul
       if(uncflag.eq.1)then
          if(t.eq.tsimul-4) then
             ix1 = 2
             txpath(t) = ix1
          else
             ix1 = 1
             txpath(t) = ix1
          end if
       else
          ix1 = 1
          txpath(t) = ix1
       end if
       ix0 = ix1

       fcumula(1) = prba(ia0,1)
       if(shocksa(t).le.fcumula(1))then
          ix1 = 1
       else
          do i=2,na
             fcumula(i) = fcumula(i-1) + prba(ia0,i)
             if(fcumula(i).gt.shocksa(t))then
                ia1 = i
                exit
             end if
          end do
       end if
       tapath(t) = ia1
       ia0 = ia1       
    end do

    do n=1,nsimul
       io0 = no/2 + 1
       iopath(n,1) = io0
       ixpath(n,1) = txpath(1)
       iapath(n,1) = tapath(1)
       do t=2,tsimul
          ix0 = txpath(t-1)
          ia0 = tapath(t)

          fcumulo(1) = prbo(io0,1,ix0)
          if(shockso(n,t).le.fcumulo(1))then
             io1 = 1
          else
             do i=2,no
                fcumulo(i) = fcumulo(i-1) + prbo(io0,i,ix0)
                if(fcumulo(i).gt.shockso(n,t))then
                   io1 = i
                   exit
                end if
             end do
          end if
          iopath(n,t) = io1
          io0 = io1

          ixpath(n,t) = txpath(t)
          iapath(n,t) = tapath(t)
       end do
    end do

  end subroutine get_simulshocks_alt

end module simulation
