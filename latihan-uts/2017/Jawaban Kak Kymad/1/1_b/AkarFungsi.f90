	real*8 function f(x) result (z)
		implicit none
		real*8 :: x

		z= ((x**2)*(exp(x))) - (2*(exp(x))) - 5

		return
	end function f

	!real*8 function f_(x) result (z)
	!	implicit none
	!	real*8 :: x
	!	z= -sin(x)-1
    !
	!	return
	!end function

	program hitung_akar
	implicit none
	real*8 :: a,b,c,toleransi
	integer :: i,itermax

	! Untuk menggunakan subroutine akar fungsi
	! fungsi yang digunakan harus "dilluar" main program
	! maka dari itu digunakan eksternal.
	! Cek cara penulisan fungsi diatas (up)
	real*8, external :: f,f_

	! Definisikan pula variabel output dari metode pencarian akar

	real*8 :: hasil


	itermax = 100
	toleransi = 1.d-5

	a=1
	b=2

	! Argument untuk subroutine mencakup seluruh
	! variabel input dan output yang dibutuhkan

	! Kali ini, f,a,b,itermax,toleransi, sebagai input
	! dan hasil sebagai output
	! Lebih lengkapnya periksa pada dokumentasi subroutine
	!call bisection(f,a,b,itermax,toleransi,hasil)
	!write(*,*)
	!call false_position(f,a,b,itermax,toleransi,hasil)
	!write(*,*)
	call secant(f,a,b,itermax,toleransi,hasil)
	write(*,*)
	!call newton_raphson(f,f_,a,itermax,toleransi,hasil)
	stop

	contains
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


	end program


