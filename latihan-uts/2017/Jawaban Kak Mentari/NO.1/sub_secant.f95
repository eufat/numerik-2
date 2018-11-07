subroutine secant(f,a_input,b_input,itermax_input,toleransi_input,c)
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
		c = b -  (f(b)*(a-b))/(f(a)-f(b))
		if (abs((c-b)) <= toleransi) then
			write(*,*) "## Metode Secant ##"
			write(*,*) "iterasi =",i
			write(*,*) "akar = ",c
			write(*,*) "nilai kesalahan = ",abs((c-b))
			write(*,*) "nilai kesalahan < toleransi"
			return
		end if
		a=b
		b=c

	end do 
	write(*,*) "## Metode Secant ##"
	write(*,*) "iterasi =", i-1
	write(*,*) "akar = ",c
	write(*,*) "nilai kesalahan = ", abs((c-b))
	return

 
end subroutine