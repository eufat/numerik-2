	program simpson
	implicit none
	integer :: i
	integer, parameter :: N=100
	real*8 w(0:N),x,xmin,xmax,dx,hasil
	
	xmin=0.d0
	xmax=2.d0
	
	dx=(xmax-xmin)/real(N,8)
	
	! Tabulasi harga pemberat Simpson
	w(0)=dx/3.d0
	do i=1,N-1,2
		w(i)=4.d0*dx/3.d0
	end do
	do i=2,N-2,2
		w(i)=2.d0*dx/3.d0
	end do
	w(N)=dx/3.d0
		
	hasil=0.d0
	do i=0,N
		x=xmin+i*dx
		hasil=hasil+w(i)*f(x)
	end do
	
	write(*,*) "Hasil integrasi simpson =",hasil
	
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