! Module Statistics
!
!
! Functions to compute mean,
! variance (sample), 
! covariance (sample),
! correlation,
! min,
! max,
! range,
! coefficient of variation
!
! IMPORTANT - missing values are assigned 1.0d20
!
!
module stats
  use prec
  real(dp), parameter:: missing = 0.9d17
contains
  real(dp) function mean(x)
    ! Computes mean of vector x and drops NaN observations
    implicit none
    real(dp) x(:),sum,num,um
    integer n,i
    n = size(x); sum = 0.0d0; num = 0.0d0; um = 1.0d0
    do i=1,n
       if(isnan(x(i))) stop 'Error: NaN'
       if(x(i).lt.missing)then
          sum = sum + x(i)
          num = num + um
       end if
       !print*,x(i)
    end do
    if(num.lt.0.5d0)then
       mean = missing*2.0d0
    else
       mean = sum/num
    end if
  end function mean

  real(dp) function cov(x,y)
    implicit none
    real(dp) x(:),y(:),mux,muy,sum,num,um
    integer nx,ny,i
    nx = size(x); ny = size(y); if(nx.ne.ny) stop 'Error: vectors of different dimension'
    mux = mean(x); muy = mean(y)
    if(mux.ge.missing.or.muy.ge.missing)then
       cov = 2.0d0*missing**2
    else
       sum = 0.0d0; num = 0.0d0; um = 1.0d0 
       do i=1,nx
          if((x(i).lt.missing).and.(y(i).lt.missing))then
             sum = sum + (x(i) - mux)*(y(i) - muy)
             num = num + um
          end if
       end do
       if(num.lt.1.5d0)then
          cov = 2.0d0*missing**2
       else
          cov = sum/(num-um)
       end if
    end if
    
  end function cov

  real(dp) function var(x)
    implicit none
    real(dp) x(:)
    var = cov(x,x)
  end function var

  real(dp) function corr(x,y)
    implicit none
    real(dp) x(:),y(:)!,mux,muy,sumxy,sumx,sumy,varx,vary,num,um
    if(var(x).gt.missing.or.abs(var(x)).lt.1.0d-10) then
       corr = 2.0d0*missing
    elseif(var(y).gt.missing.or.abs(var(y)).lt.1.0d-10) then
       corr = 2.0d0*missing
    elseif(cov(x,y).gt.missing)then
       corr = 2.0d0*missing
    else
       corr = cov(x,y)/sqrt(var(x)*var(y))
    end if
    
    !integer nx,ny,i
    ! nx = size(x); ny = size(y); if(nx.ne.ny) stop 'Error: vectors of different dimension'
    ! num = 0.0d0; um = 1.0d0 
    ! sumx = 0.0d0
    ! sumy = 0.0d0
    ! do i=1,nx
    !    if((x(i).lt.missing).and.(y(i).lt.missing))then
    !       sumx = sumx + x(i)
    !       sumy = sumy + y(i)
    !       num = num + um
    !    end if
    ! end do
    ! if(num.lt.0.5d0)then
    !    corr = missing*2.0d0
    ! else
    !    mux = sumx/num; muy = sumy/num
    !    sumxy = 0.0d0; sumx = 0.0d0; sumy = 0.0d0
    !    do i=1,nx
    !       if((x(i).lt.missing).and.(y(i).lt.missing))then
    !          sumxy = sumxy + (x(i) - mux)*(y(i) - muy)
    !          sumx  = sumx  + (x(i) - mux)**2
    !          sumy  = sumy  + (y(i) - muy)**2
    !       end if
    !    end do
    !    if(sumx.eq.0.0d0.or.sumy.eq.0.0d0)then
    !       corr = missing*2.0d0
    !    else
    !       corr = sumxy/sqrt(sumx*sumy)
    !    end if
    ! end if
  end function corr

end module stats
