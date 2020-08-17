program gplot
  use prec; use parameters; use uteis;
  implicit none

  integer, parameter:: nnk=5000
  ! main variables
  real(dp) &
       k(nk),o(no),prbo(no,no),e(ne),prbe(ne,ne), &
       v(nk,no,ne),vg(nk,no,ne),expec(nk,no)
  integer kp(nk,no,ne),kfact
  ! aux vars
  real(dp) kkv(nnk)
  integer ik,ik1,ik2,ik3,io,io1,io2,io3,ie,ie1,ie2,ie3
  ! interpolation
  real(dp) &
       tk( nk+kk,3),to( no+ko,3), &
       ttk(nk+kk),  tto(no+ko), &
       work( nk*no + max(2*kk*(nk+1),2*ko*(no+1)),3), &
       wwork(nk*no + max(2*kk*(nk+1),2*ko*(no+1))), &
       bcoef( nk,no,3), &
       bbcoef(nk,no) , &
       work2s( 3*max(kk,ko)+ko,3), &
       wwork2s(3*max(kk,ko)+ko)
  real(dp) db2val
  integer iflag

  open(1,file='functions/kp.txt', position="rewind");read(1,*) kp;close(1)
  open(1,file='functions/kfact.txt', position="rewind");read(1,*) kfact;close(1)
  open(1,file='functions/v.txt', position="rewind");read(1,*) v;close(1)
  open(1,file='functions/expec.txt', position="rewind");read(1,*) expec;close(1)
  open(1,file='functions/k.txt', position="rewind");read(1,*) k;close(1)
  open(1,file='functions/o.txt', position="rewind");read(1,*) o;close(1)
  open(1,file='functions/e.txt', position="rewind");read(1,*) e;close(1)

  open(2,file='plot_data/vg_k.txt', position="rewind")
  open(3,file='plot_data/evg_k.txt', position="rewind")
  open(4,file='plot_data/dvg_k.txt', position="rewind")
  open(5,file='plot_data/devg_k.txt', position="rewind")
  open(6,file='plot_data/xvg_k.txt', position="rewind")
  open(7,file='plot_data/xevg_k.txt', position="rewind")

  call grid(nnk,kmin,kmax,kkv)

  iflag = 0; call db2ink(k,nk,o,no,v(:,:,1),nk,kk,ko,tk(:,1),to(:,1),bcoef(:,:,1),work(:,1),iflag)
  iflag = 0; call db2ink(k,nk,o,no,v(:,:,2),nk,kk,ko,tk(:,2),to(:,2),bcoef(:,:,2),work(:,2),iflag)
  iflag = 0; call db2ink(k,nk,o,no,v(:,:,3),nk,kk,ko,tk(:,3),to(:,3),bcoef(:,:,3),work(:,3),iflag)
  iflag = 0; call db2ink(k,nk,o,no,expec   ,nk,kk,ko,ttk    ,tto    ,bbcoef      ,wwork    ,iflag)

  ie1 = 2
  do ik=1,nk
     write(6,'(6(f16.8))') k(ik),&
          v(ik,1     ,ie1), &
          v(ik,no/3  ,ie1), &
          v(ik,no/2+1,ie1), &
          v(ik,no-3  ,ie1), &
          v(ik,no    ,ie1)
     write(7,'(6(f16.8))') k(ik),&
          expec(ik,1), &
          expec(ik,no/3), &
          expec(ik,no/2+1), &
          expec(ik,no-3), &
          expec(ik,no)
  end do

  do ik=1,nnk
     write(2,'(6(f16.8))') kkv(ik),&
          db2val(kkv(ik),o(1)     ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/3)  ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/2+1),0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no-3)  ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no)    ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2))
     write(4,'(6(f16.8))') kkv(ik),&
          db2val(kkv(ik),o(1)     ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/3)  ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/2+1),1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no-3)  ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no)    ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2))
     ! write(2,'(6(f16.8))') kkv(ik),&
     !      db2val(kkv(ik),(o(1)+o(2))/2d0     ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
     !      db2val(kkv(ik),(o(no/3)+o(no/3+1)/2d0)  ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
     !      db2val(kkv(ik),(o(no/2+1)+o(no/2+2))/2d0,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
     !      db2val(kkv(ik),(o(no-3)+o(no-2))/2d0  ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
     !      db2val(kkv(ik),(o(no)+o(no-1))/2d0    ,0,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2))

  end do

  do ik=1,nnk
     write(4,'(6(f16.8))') kkv(ik),&
          db2val(kkv(ik),o(1)     ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/3)  ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no/2+1),1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no-3)  ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2)), &
          db2val(kkv(ik),o(no)    ,1,0,tk(:,2),to(:,2),nk,no,kk,ko,bcoef(:,:,2),work2s(:,2))
     write(3,'(6(f16.8))') kkv(ik),&
          db2val(kkv(ik),o(1)     ,0,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no/3)  ,0,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no/2+1),0,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no-3)  ,0,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no)    ,0,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s)
     write(5,'(6(f16.8))') kkv(ik),&
          db2val(kkv(ik),o(1)     ,1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no/3)  ,1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no/2+1),1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no-3)  ,1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s), &
          db2val(kkv(ik),o(no)    ,1,0,ttk,tto,nk,no,kk,ko,bbcoef,wwork2s)
  end do

  close(2); close(3); close(4); close(5); close(6); close(7)
end program gplot
