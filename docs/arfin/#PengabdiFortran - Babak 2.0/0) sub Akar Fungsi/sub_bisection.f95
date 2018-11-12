subroutine bisection(f,a_input,b_input,itermax_input,toleransi_input,c)
	integer, optional,intent(in) :: itermax_input
	real*8 , optional, intent(in) :: toleransi_input
	real*8, intent(in) :: a_input,b_input
	real*8 :: c,toleransi,a,b
	real*8, external :: f
	integer :: i, itermax
	
	! Dummy variable to input
	a=a_input
	b=b_input
	itermax = itermax_input
	toleransi = toleransi_input
	
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
			write(*,*) "## Metode Bisection ##"
			write(*,*) "iterasi = ",i
			write(*,*) "akar = ",c
			write(*,*) "nilai kesalahan =", abs(a-b)
			return
		end if
	end do
	write(*,*) "## Metode Bisection ##"
	write(*,*) "iterasi = ",i-1
	write(*,*) "akar = ",c
	write(*,*) "nilai kesalahan  =", abs(a-b)
	return
	
	
	end subroutine