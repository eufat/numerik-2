	real*8 function f(x) result(z)
	implicit none
	real*8, intent(in) :: x
	z=5.d0*x**3-2.d0*x**2+3.d0*x-10.d0 !###### GANTI WEY ######!
	return
	end function	
	
	program main
	implicit none
	integer:: N
	integer :: i
	real*8 :: xmin,xmax,hasil
	
	real*8, external :: f,simpson,trapezoid !###### GANTI WEY ######!
	
	xmin=0.d0 !###### GANTI WEY ######!
	xmax=10.d0 !###### GANTI WEY ######!
	N=100 !###### GANTI WEY ######!
	
	hasil = simpson(f,xmin,xmax,N)

	
	!Tulis hasil integrasi
	write(*,*) "Hasil integrasi =",hasil
	
	hasil = trapezoid(f,xmin,xmax,N)

	
	!Tulis hasil integrasi
	write(*,*) "Hasil integrasi =",hasil
	
	stop
	
	end program