	real*8 function int_lagrange3(t_,N_,xd_,fd_) result(z_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: xd_(N_),fd_(N_),t_
	integer :: i,j,k,N,M,over,ki,ka,c,h
	real*8, allocatable :: xd(:), fd(:),A(:,:),B(:),X_(:),At(:),Ab(:,:),Bb(:),Bt,sum
	real*8 ::  p, x
	N = N_
	allocate(xd(N))
	allocate(fd(N))
	allocate(A(4,4))
	allocate(B(4))
	allocate(X_(4))
	allocate(At(4))
	allocate(Ab(4,4))
	allocate(Bb(4))
	xd = xd_
	fd = fd_

	x = t_

	do c=1,N
		if (x<=xd(c)) then
			over=0
			exit
		end if
		over=1
	end do
	
	if (c==1 .OR. over==1) then
		write(*,*) "Titik tinjau di luar  batas!"
		z_=0
		return
	else if(c==2) then
		ki=c-1
		ka=c+2
	else if(c==N) then
		ki=c-3
		ka=c
	else
		ki = c-2
		ka = c+1
	end if
	
	i=1
	do h=ki,ka
		do j=1,4
			A(i,j) = xd(h)**(j-1)
		end do
		i=i+1
	end do
	
	
	i=1
	do h=ki,ka
		B(i) = fd(h)
		i=i+1
	end do
	
	N=4
	Ab=A
	Bb=B

! Proses triangulasi matrix A
	do k=1,N-1
		do i=k+1,N
			do j=k,N
				Ab(i,j)=A(i,j)-(A(i,k)/A(k,k))*A(k,j)
			end do
			Bb(i)=B(i)-(A(i,k)/A(k,k))*B(k)
		end do
		! Cek apakah A(k,k) berharga nol
		if (Ab(k+1,k+1)==0.d0) then
		! Tukar isi matrix A baris ke-k isi baris di bawahnya
			At(:)=Ab(k+1,:)
			Bt=Bb(k+1)
			Ab(k+1,:)=Ab(k+2,:)
			Ab(k+2,:)=At(:)
			Bb(k+1)=Bb(k+2)
			Bb(k+2)=Bt
		end if
		A=Ab
		B=Bb
	end do
			
	! Substitusi mundur	
	X_(N)=B(N)/A(N,N)
	do j=1,N-1
		sum=0.d0
		do k=N-j+1,N
			sum=sum+A(N-j,k)*X_(k)
		end do
		X_(N-j)=(B(N-j)-sum)/A(N-j,N-j)
	end do
	
	write(*,*)
	write(*,*) "Koefisien Polinomial Lagrange:"
	do i=1,N
		write(*,*) "a(",i-1,")= ",X_(i)
	end do
	write(*,*)
	p=0
	do i=0,N-1
			p = p + (X_(i+1))*(x**i)
	end do

	z_ = p	
	
	deallocate(xd)
	deallocate(fd)
	return
	end function