	program bisection
	implicit none
	real*8 :: a,b,c,toleransi
	integer :: i,itermax
	
	itermax = 100
	toleransi = 1.d-13
	
	
	a=0.0d0
	b=10.0d0
	
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
	
	z =  -0.8099949478439129*x**2 + 3.134961752206575*x+5.084635798983653
	
	return
	end function
	
	end program bisection