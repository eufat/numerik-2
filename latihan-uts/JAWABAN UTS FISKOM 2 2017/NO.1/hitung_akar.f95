	real*8 function f(x) result (z)
		implicit none
		real*8 :: x
		
		z= x*x - 2 - 5*exp(-x) ! GANTI SESUAI SOAL
		
		return
	end function f
	
	real*8 function f_(x) result (z)
		implicit none
		real*8 :: x
		z= 2*x + 5*exp(-x) ! TURUNAN DARI FUNGSI DARI SOAL
		
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
	
	
	itermax = 100 ! TEBAK SENDIRI
	toleransi = 1.d-5 
	
	a=0d0 ! TEBAK SENDIRI
	b=100.0d0 ! TEBAK SENDIRI
	
	! Argument untuk subroutine mencakup seluruh
	! variabel input dan output yang dibutuhkan
	
	! Kali ini, f,a,b,itermax,toleransi, sebagai input
	! dan hasil sebagai output
	! Lebih lengkapnya periksa pada dokumentasi subroutine
	! YANG DI BAWAH PILIH SENDIRI PROGRAM YANG MANA
	!call bisection(f,a,b,itermax,toleransi,hasil)
	!write(*,*) 
	!call false_position(f,a,b,itermax,toleransi,hasil)
	!write(*,*) 
	call secant(f,a,b,itermax,toleransi,hasil)
	write(*,*) 
	!call newton_raphson(f,f_,a,itermax,toleransi,hasil)
	!write(*,*)
	stop
	
	end program 
	

