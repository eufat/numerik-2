	real*8 function f_genap(x) result (z)
		implicit none
		real*8 :: x
		
		z= (sqrt(10-(x**2)) /x) - tan(x)
		
		return
	end function 
		
	real*8 function f_ganjil(x) result (z)
		implicit none
		real*8 :: x
		
		z= (sqrt(10-(x**2)) /x) + (1/tan(x))
		
		return
	end function 
	
	program main
	implicit none
	real*8 :: a,b,c,toleransi
	integer :: i,itermax
	
	! Untuk menggunakan subroutine akar fungsi
	! fungsi yang digunakan harus "dilluar" main program
	! maka dari itu digunakan eksternal.
	! Cek cara penulisan fungsi diatas (up)
	real*8, external :: f_genap,f_ganjil
	
	! Definisikan pula variabel output dari metode pencarian akar
	
	real*8 :: y_genap,y_ganjil,E_genap,E_ganjil
	
	
	itermax = 10000
	toleransi = 1.d-15
	
	a=0
	b=1.5
	call bisection(f_genap,a,b,itermax,toleransi,y_genap)
	
	a=2
	b=3
	call bisection(f_ganjil,a,b,itermax,toleransi,y_ganjil)
	
	E_genap = (y_genap**2)-10
	E_ganjil = (y_ganjil**2)-10
	write(*,*)
	write(*,*) "y pada paritas genap :"
	write(*,*) y_genap
	write(*,*) "E pada paritas genap :"
	write(*,*) E_genap
	write(*,*)
	write(*,*) "y pada paritas ganjil :"
	write(*,*) y_ganjil
	write(*,*) "E pada paritas ganjil :"
	write(*,*) E_ganjil
	stop
	
	end program 
	

