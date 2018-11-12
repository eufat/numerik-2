	program bisection
	implicit none
	real*8 :: a,b,c,toleransi
	integer :: i,itermax
	
	itermax = 10000
	toleransi = 1.d-13
	
	
	a=112
	b=120
	
	if (f(a)*f(b) > 0 )then
		write(*,*) "Akar tidak berada pada rentang", a ,"sampai", b
		stop
	end if
	
	do i=1,itermax
		c=0.5d0*(a+b)
		if (f(a)*f(c)<0.d0) then
			b=c
		else 
			a=c
		end if
				
		if (abs(a-b)<= toleransi) then
			write(*,*) "iterasi = ",i
			write(*,*) "akar = ",c
			write(*,*) "nilai kesalahan =", abs(a-b)
			stop
		end if
	end do
	
	write(*,*) "iterasi = ",i-1
	write(*,*) "akar = ",c
	write(*,*) "nilai kesalahan  =", abs(a-b)
	stop
	
	contains
	
	function f(x) result(z)
	implicit none
	real*8, intent (in) :: x
	real*8 :: z
	
	z = -0.0000328776041666667*x**3+0.01170312500000001*x**2 -1.4153958333333343*x+58.1170000000000
	
	return
	end function
	
	end program bisection