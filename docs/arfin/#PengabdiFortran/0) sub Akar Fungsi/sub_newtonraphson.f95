subroutine newton_raphson(f,f_,a_input,itermax_input,toleransi_input,c)
		integer, optional,intent(in) :: itermax_input
		real*8 , optional, intent(in) :: toleransi_input
		real*8, intent(in) :: a_input
		real*8 :: a,c,toleransi,akar
		real*8,external ::f,f_
		integer :: i,itermax
		
		! Dummy variable to input
		a=a_input
		itermax = itermax_input
		toleransi = toleransi_input

		
		do i=1,itermax
			c=a-(f(a)/f_(a))

			if (abs(a-c)<=toleransi) then
				akar = c
				write(*,*) "## Metode Newton Raphson ##"
				write(*,*) "iterasi =", i
				write(*,*) "akar = ",c
				write(*,*) "nilai kesalahan = ", abs(a-c)
				return
			end if	
			a = c
		end do
		write(*,*) "## Metode Newton Raphson ##"
		write(*,*) "iterasi =", i-1
		write(*,*) "akar = ",c
		write(*,*) "nilai kesalahan = ", abs(a-c)
		return
					
	end subroutine newton_raphson	