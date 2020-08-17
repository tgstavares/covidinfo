program main
  use prec; use parameters; use process_alt; use globals
  use discrete_shock; use omp_lib
  implicit none

  ! main variables
  real(dp) &
       k(nk),o(no),prbo(no,no),e(ne),prbe(ne,ne), &
       v(nk,no,ne),vg(nk,no,ne),expec(nk,no)
  integer kp(nk,no,ne),kfact
  ! Auxiliary variables
  real(dp) dif,vcurr,vmax,prcurr
  real(dp) pprcurr(nk,no),iinv(nk,nk,ne)
  integer ik,io,ie,iter,j,i,imax
  real(8) t1,t2

  t1 = omp_get_wtime()
  
  ! Grids and omega/ shock
  !call tauchen86(rho_o,sigma_o,no,4d0,prbo,o)
  call geto(rho_o,sigma_o,no,omin,omax,prbo,o); o = o + mu_o
  call getk(nk,kmin,kmax,delta,k,kfact)
  print*,'Load new guess'
  call compvg(pk,c2,k,o,vg)
  !call gete(mu_e,rho_e,sigma_e,ne,prbe,e)
  call tauchen86(rho_e,sigma_e,ne,4d0,prbe,e)!;e = e - sigma_e**2/2d0

  do ik=1,nk
     do io=1,no
        pprcurr(ik,io)=profit(k(ik),o(io))
     end do
     do ie=1,ne
        do i=1,nk
           iinv(i,ik,ie) = invadj(pk,c2,k(ik),k(i))*exp(e(ie))
        end do
     end do
  end do

  ! Iterations
  iter = 1; dif=1.0; print*,'Running......'
  do while(dif.gt.erro2)
     do io=1,no
        do ik=1,nk
           expec(ik,io) = 0d0
           do j=1,ne
              expec(ik,io) = expec(ik,io) + prbe(1,j)*sum(vg(ik,:,j)*prbo(io,:))
           end do
        end do
     end do

     do ie=1,ne
        do io=1,no
           imax = kfact+1
           do ik=1,nk
              vmax  = -infty
              do i=max(ik-kfact,imax),nk
                 vcurr = pprcurr(ik,io) - iinv(i,ik,ie) + beta*expec(i,io)
                 if(vcurr-vmax.gt.erro1)then
                    vmax = vcurr; imax = i
                 else
                    exit
                 end if
              end do
              v(ik,io,ie) = vmax
              kp(ik,io,ie)= imax
           end do
        end do
     end do
     dif = maxval(abs(vg-v))
     vg = v
     write(*,*) dif,iter
     iter = iter + 1
  end do

  t2 = omp_get_wtime()
  print*,'Program execution time: ',t2-t1

  open(1,file='functions/kp.txt', position="rewind");write(1,*) kp;close(1)
  open(1,file='functions/kfact.txt', position="rewind");write(1,*) kfact;close(1)
  open(1,file='functions/v.txt', position="rewind");write(1,*) v;close(1)
  open(1,file='functions/expec.txt', position="rewind");write(1,*) expec;close(1)
  open(1,file='functions/k.txt', position="rewind");write(1,*) k;close(1)
  open(1,file='functions/o.txt', position="rewind");write(1,*) o;close(1)
  open(1,file='functions/e.txt', position="rewind");write(1,*) e;close(1)
  open(1,file='functions/prbo.txt', position="rewind");write(1,*) prbo;close(1)
  open(1,file='functions/prbe.txt', position="rewind");write(1,*) prbe;close(1)  
end program main
