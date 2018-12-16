	program trapezoid
	implicit none
	integer :: i
	integer, parameter :: N=100
	real*8 x(0:N),w(0:N),xmin,xmax,dx,hasil
	
	xmin=0.d0
	xmax=2.d0
	
	dx=(xmax-xmin)/real(N,8)
	
	!Tabulasi harga-harga x(i)
	x(0)=xmin
	x(N)=xmax
	do i=1,N-1
		x(i)=x(0)+real(i,8)*dx
	end do
	
	!Tabulasi harga pemberat trapezoid w(i)
	w(0)=dx/2.d0
	w(N)=dx/2.d0
	do i=1,N-1
		w(i)=dx
	end do
	
	hasil=0.d0
	do i=0,N
		hasil=hasil+f(x(i))*w(i)
	end do
	
	write(*,*) "Hasil integrasi trapezoid =",hasil
	
	stop
	
	contains 
	
	function f(x) result(z)
	implicit none
	real*8, intent(in) :: x
	real*8 :: z,pi
	pi=2.d0*asin(1.d0)
	z=exp(0.5d0*x)*sin(x)  	! coba 1
	return
	end function
	
	end program