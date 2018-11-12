 subroutine false_position(f,a_input,b_input,itermax_input,toleransi_input,c)
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

	
	do i=1,itermax
		c=(a*f(b) - b*f(a))/(f(b)-f(a))
		if (f(a)*f(c)<0.d0) then
			b=c
		else
			a=c
		end if
		if (abs(c-c_old)<=toleransi) then
			write(*,*) "## Metode False Position ##"
			write(*,*) "iterasi =", i
			write(*,*) "akar = ",c
			write(*,*) "nilai kesalahan = ", abs(c-c_old)
			return
		end if	
	c_old = c
	end do
	
	write(*,*) "## Metode False Position ##"
	write(*,*) "iterasi =", i-1
	write(*,*) "akar = ",c
	write(*,*) "nilai kesalahan = ", abs(c-c_old)
	return
	end subroutine