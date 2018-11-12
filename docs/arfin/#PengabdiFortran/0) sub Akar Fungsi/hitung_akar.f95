	real*8 function f(x) result (z)
		implicit none
		real*8 :: x
		
		z= cos(x)-x
		
		return
	end function f
	
	real*8 function f_(x) result (z)
		implicit none
		real*8 :: x
		z= -sin(x)-1
		
		return 
	end function
	
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
	
	a=0d0
	b=5.0d0
	
	! Argument untuk subroutine mencakup seluruh
	! variabel input dan output yang dibutuhkan
	
	! Kali ini, f,a,b,itermax,toleransi, sebagai input
	! dan hasil sebagai output
	! Lebih lengkapnya periksa pada dokumentasi subroutine
	call bisection(f,a,b,itermax,toleransi,hasil)
	write(*,*) 
	call false_position(f,a,b,itermax,toleransi,hasil)
	write(*,*) 
	call secant(f,a,b,itermax,toleransi,hasil)
	write(*,*) 
	call newton_raphson(f,f_,a,itermax,toleransi,hasil)
	stop
	
	end program 
	

