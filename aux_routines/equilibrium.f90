module equilibrium
  use prec; use parameters; use globals; use bspline_module
contains

  subroutine get_equil(p,m,showoutput)
    use omp_lib; use values
    implicit none
    type(param), intent(in):: p
    type(model), intent(inout):: m
    character(len=1), intent(in), optional::showoutput
    integer flag_showoutput,iflag
    integer ix0,ix1,ik0,ia0,iz0,ik1,ia1,iz1,i,ib0,ib1
    real(dp) difv,k0,a0,z0,k1,expec,cashflow,b1,b0, &
         v,vi,kpi,bpi,va,bpa,kpa,kpguess,bpguess,kp,bp,bpupp,rr
    real(dp) aux1,aux2
    real(dp) w1(kb),w0(3*max(kk,kb))
    integer inbvx,inbvy,iloy


    ! Print or skip output
    flag_showoutput = 1
    if(present(showoutput))then
       if(showoutput(1:1).eq.'n'.or.showoutput(1:1).eq.'No') flag_showoutput = 0
    end if

    if((flag_showoutput.eq.1)) print*,""    
    ! Do value function itererations
    difv = 1d0
    do while(difv.gt.erro4)
       m%iter = m%iter + 1
       if(m%hflag.gt.0) m%hflag = m%hflag - 1

       ! ! Howard acceleration
       ! if(m%iter.gt.1.and.naccel.gt.0.and.m%hflag.eq.0.and.m%jflag.eq.0.and.m%iter.le.15)then
       !    do i=1,naccel             

       !       !$omp parallel do collapse(3) private(ix0,ia0,iz0,ib1,ik1,expec,ix1,ia1,iz1,iflag)
       !       do ix0=1,nx
       !          do ia0=1,na
       !             do iz0=1,nz
       !                do ib1=1,nb
       !                   do ik1=1,nk
       !                      expec = 0d0
       !                      do ix1=1,nx
       !                         do ia1=1,na
       !                            do iz1=1,nz
       !                               expec = expec + m%prbz(iz0,iz1,ix0)*m%prbx(ix0,ix1)*m%prba(ia0,ia1) * m%vg(ik1,ib1,iz1,ia1,ix1)
       !                            end do
       !                         end do
       !                      end do
       !                      m%evg(ik1,ib1,iz0,ia0,ix0) = expec
       !                   end do
       !                end do
       !                call db2ink(m%k,nk,m%b,nb,m%evg(:,:,iz0,ia0,ix0),kk,kb,1,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),iflag)
       !             end do
       !          end do
       !       end do
       !       !$omp end parallel do

       !       !$omp parallel do collapse(5) private(ix0,ia0,iz0,ib0,ik0,a0,z0,k0,b0,k1,b1,expec,cashflow, iflag,inbvx,inbvy,iloy,w1,w0)
       !       do ix0=1,nx
       !          do ia0=1,na
       !             do iz0=1,nz
       !                do ib0=1,nb
       !                   do ik0=1,nk
       !                      a0  = m%a(ia0)
       !                      z0  = m%z(iz0)
       !                      k0  = m%k(ik0)
       !                      b0  = m%b(ib0)
       !                      k1  = m%kp(ik0,ib0,iz0,ia0,ix0)
       !                      !b1  = m%bp(ik0,ib0,iz0,ia0,ix0)
       !                      b1  = min(m%bp(ik0,ib0,iz0,ia0,ix0),borrowconstraint(p,k1))
       !                      inbvx=1;inbvy=1;iloy=1
       !                      call db2val(k1,b1,0,0,m%tk,m%tb,nk,nb,kk,kb,m%bsevg(:,:,iz0,ia0,ix0),expec,iflag,inbvx,inbvy,iloy,w1,w0)
       !                      if(abs(k1-(1d0-deltak)*k0).le.erro8)then
       !                         cashflow = cashflowfn(p,k0,b0,z0,a0,k1,b1)
       !                      else
       !                         cashflow = cashflowfn(p,k0,b0,z0,a0,k1,b1) - nconvexk(p,k0)
       !                      end if
       !                      m%vc(ik0,ib0,iz0,ia0,ix0) = cashflow - issuecosts(cashflow) + beta*expec
       !                   end do
       !                end do
       !             end do
       !          end do
       !       end do
       !       !$omp end parallel do
       !       m%vg = m%vc

       !    end do
       ! end if


       ! ! Compute expected value
       ! !$omp parallel do collapse(3) private(ix0,ia0,iz0,ib1,ik1,expec,ix1,ia1,iz1,iflag)
       ! do ix0=1,nx
       !    do ia0=1,na
       !       do iz0=1,nz
       !          do ib1=1,nb
       !             do ik1=1,nk
       !                expec = 0d0
       !                do ix1=1,nx
       !                   do ia1=1,na
       !                      do iz1=1,nz
       !                         expec = expec + m%prbz(iz0,iz1,ix0)*m%prbx(ix0,ix1)*m%prba(ia0,ia1) * m%vg(ik1,ib1,iz1,ia1,ix1)
       !                      end do
       !                   end do
       !                end do
       !                m%evg(ik1,ib1,iz0,ia0,ix0) = expec
       !             end do
       !          end do
       !          call db2ink(m%k,nk,m%b,nb,m%evg(:,:,iz0,ia0,ix0),kk,kb,1,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),iflag)
       !       end do
       !    end do
       ! end do
       ! !$omp end parallel do
       
       ! Compute expected value
       !$omp parallel do collapse(3) private(ix0,ia0,iz0,ib1,ik1,expec,ix1,ia1,iz1,iflag)
       do ix0=1,nx
          do ia0=1,na
             do iz0=1,nz
                call get_evg_qb(m%k,m%b,m%tk,m%tb,m%prbz(iz0,:,ix0),m%prba(ia0,:),m%prbx(ix0,:),m%vg,m%bsevg(:,:,iz0,ia0,ix0),m%bsqb(:,:,iz0,ia0,ix0))
             end do
          end do
       end do
       !$omp end parallel do

       aux1 = 10000d0
       aux2 = 10000d0

       !$omp parallel do collapse(5) private(ix0,ia0,iz0,ib0,ik0,a0,z0,b0,k0,bpguess,kpguess,kpa,bpa,va,kpi,bpupp,bpi,v,kp,bp,cashflow,vi) reduction(min:aux1) reduction(min:aux2)
       do ix0=1,nx
          do ia0=1,na
             do iz0=1,nz
                do ib0=1,nb
                   do ik0=1,nk
                      a0  = m%a(ia0)
                      z0  = m%z(iz0)
                      b0  = m%b(ib0)
                      k0  = m%k(ik0)

                      kpguess = m%kp(ik0,ib0,iz0,ia0,ix0)
                      bpguess = min(m%bp(ik0,ib0,iz0,ia0,ix0),borrowconstraint(p,kpguess))

                      call get_maxkb(p,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),m%bsqb(:,:,iz0,ia0,ix0),kmin,kmax,kpguess,bpguess,k0,b0,z0,a0,kpa,bpa,va)
                      !call get_maxkb(p,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),kmin,kmax,kpguess,bpguess,k0,b0,z0,a0,kpa,bpa,va)

                      ! print*,va,kpa,bpa
                      ! stop

                      kpi = (1d0 - deltak)*k0
                      if(kpi.lt.kmin)then
                         vi = -10000d0
                         v  = va
                         kp = kpa
                         bp = bpa
                         rr = r_p
                         cashflow = cashflowfn(p,k0,b0,z0,a0,kp,bp,rr) - nconvexk(p,k0)

                      else
                         bpupp = borrowconstraint(p,kpi)
                         
                         call get_maxb(p,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),m%bsqb(:,:,iz0,ia0,ix0),bpupp,k0,b0,z0,a0,kpi,bpi,vi)
                         !call get_maxb(p,m%tk,m%tb,m%bsevg(:,:,iz0,ia0,ix0),bpupp,k0,b0,z0,a0,kpi,bpi,vi)

                         if(vi.gt.va)then
                            v  = vi
                            kp = kpi
                            bp = bpi
                            rr = r_p
                            cashflow = cashflowfn(p,k0,b0,z0,a0,kp,bp,rr)
                         else
                            v  = va
                            kp = kpa
                            bp = bpa
                            rr = r_p
                            cashflow = cashflowfn(p,k0,b0,z0,a0,kp,bp,rr) - nconvexk(p,k0)
                         end if
                      end if
                      m%vc(ik0,ib0,iz0,ia0,ix0) = v
                      m%kp(ik0,ib0,iz0,ia0,ix0) = kp
                      m%bp(ik0,ib0,iz0,ia0,ix0) = bp

                      if(cashflow.le.aux1) aux1 = cashflow
                      if((borrowconstraint(p,kp)-bp).le.aux2) aux2 = (borrowconstraint(p,kp)-bp)

                      ! write(*,'(5i7,13f18.4)') &
                      !      ix0,ia0,iz0,ib0,ik0,a0,z0,b0,k0,&
                      !      kp,kp-(1d0-deltak)*k0,(kp-(1d0-deltak)*k0)/k0,bp, &
                      !      cashflow,v
                      ! stop
                   end do
                   !stop
                end do
             end do
          end do
       end do
       !$omp end parallel do

       ! print*,aux1
       ! stop

       !difv = maxval(abs(m%vc-m%vg))
       !difv = sum(abs(m%vc-m%vg))/dble(nk*nb*nz*nx)
       !difv = maxval(abs(m%vc-m%vg)/m%vg)
       difv = sum(abs(m%vc-m%vg)/abs(m%vg))/dble(nk*nb*nz*na*nx)

       m%vg = m%vc
       if(difv.le.erro4*8d0)then
          m%jflag = 1
       else
          m%jflag = 0
       end if
       if((flag_showoutput.eq.1)) &
            write(*,'(3i5,f17.7,12f17.2)') &
            m%iter,m%hflag,m%jflag,difv, &
            minval(m%kp),m%k(1),maxval(m%kp),m%k(nk), &
            minval(m%bp),m%b(1),maxval(m%bp),m%b(nb), &
            minval(m%vg),aux1,aux2
       if(m%iter.gt.nvfi) exit
       if(mod(m%iter,10).eq.0.and.m%iter.gt.2.and.m%hflag.eq.0) then
          m%hflag = 32
       end if

       !stop 'c1'

    end do
    if((flag_showoutput.eq.1)) print*,""

  end subroutine get_equil
end module equilibrium
