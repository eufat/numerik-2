	program simpson
	implicit none
	integer :: i
	integer, parameter :: N=100
	real*8 w(0:N),x,xmin,xmax,dx,hasil
	
	xmin=0.d0
	xmax=10.d0
	
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
	
	write(*,*) "Panjang lintasan =",hasil
	write(*,*) "Panjang lintasan =",hasil/10
	
	
	stop
	
	contains 
	
	function f(x) result(z)
	implicit none
	real*8, intent(in) :: x
	real*8 :: z,pi,a0,a1,a2,a3,a4,a5
	a0=8.166190451657624
	a1=7.751371914407182
	a2=1.5050440090104802
	a3=0.15608923354044124
	a4=-0.10814451284178714
	a5=0.00702867271048242
	z = a0 + a1*x + a2*x**2 + a3*x**3 + a4*x**4 + a5*x**5
	return
	end function
	
	end program