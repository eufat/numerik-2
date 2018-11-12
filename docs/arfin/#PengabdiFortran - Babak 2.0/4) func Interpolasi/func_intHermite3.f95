	real*8 function int_hermite3(t_,N_,xd_,fd_) result(z_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: xd_(N_),fd_(N_),t_
	integer :: i,j,k,N,M,over,ki,ka,c,h
	real*8, allocatable :: xd(:), fd(:),A(:,:),B(:),X_(:),At(:),Ab(:,:),Bb(:),Bt,sum
	real*8 ::  p, x,x0,x1,x2,x3,fx0,fx1,fx2,fx3,f_1,f_2
	real*8, external :: f_
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
		return
	else if(c==2) then
			x1	=	xd(c-1)
			x2	=	xd(c)
			x3	=	xd(c+1)

			fx1	=	fd(c-1)
			fx2	=	fd(c)
			fx3	=	fd(c+1)
			
			f_1 = f_(x1,x1,x2,x3,fx1,fx2,fx3) 
			f_2 = f_(x2,x1,x2,x3,fx1,fx2,fx3)	
	else if(c==N) then
			x0	=	xd(c-2)
			x1	=	xd(c-1)
			x2	=	xd(c)

			fx0	=	fd(c-2)
			fx1	=	fd(c-1)
			fx2	=	fd(c)
			
			f_1 = f_(x1,x0,x1,x2,fx0,fx1,fx2) 
			f_2 = f_(x2,x0,x1,x2,fx0,fx1,fx2)
	else
			x0	=	xd(c-2)
			x1	=	xd(c-1)
			x2	=	xd(c)
			x3	=	xd(c+1)
			fx0	=	fd(c-2)
			fx1	=	fd(c-1)
			fx2	=	fd(c)
			fx3	=	fd(c+1)
			
			f_1 = f_(x1,x0,x1,x2,fx0,fx1,fx2) 
			f_2 = f_(x2,x1,x2,x3,fx1,fx2,fx3)
	end if
	

	do j=1,4
		A(1,j) = x1**(j-1)
	end do
	
	do j=1,4
		A(2,j) = x2**(j-1)
	end do
	
	do j=2,4
		A(3,j) = (j-1)*x1**(j-2)
	end do
	
	do j=2,4
		A(4,j) = (j-1)*x2**(j-2)
	end do
	
	A(3,1) = 0
	A(4,1) = 0

	B(1) = fx1
	B(2) = fx2
	B(3) = f_1
	B(4) = f_2
	
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
	write(*,*) "Koefisien Polinomial Hermite:"
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
	
	real*8 function f_(x_,x_0,x_1,x_2,fx_0,fx_1,fx_2) result (z)
		real*8 :: x_0,x_1,x_2,fx_0,fx_1,fx_2,x_
			
		z = ((2*x_-x_1-x_2)/((x_0-x_1)*(x_0-x_2)))*fx_0+ &
       ((2*x_-x_0-x_2)/( (x_1-x_0)*(x_1-x_2)))*fx_1+ &
       ((2*x_-x_0-x_1)/((x_2-x_0)*(x_2-x_1)))*fx_2;
		
		
		return
	end function f_
	