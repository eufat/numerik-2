	program simpson
!-------------------------------------------
! Keterangan:
! Program untuk menghitung integral f(x)dx 
! dengan batas-batas tertentu xmin dan xmax.
! Hasil integrasi hanya berupa angka yang 
! disimpan dalam variabel "hasil".
!-------------------------------------------
	implicit none
	integer :: i
	integer, parameter :: N=100
	real*8 w(0:N),x,xmin,xmax,dx,hasil
	
	xmin=0.d0
	xmax=2.d0
	
	dx=(xmax-xmin)/real(N,8)
	
	! Tabulasi harga pemberat Simpson
	w(0)=dx/3.d0
	do i=1,N-1,2
		w(i)=4.d0*dx/3.d0
	end do
	do i=2,N-2,2
		w(i)=2.d0*dx/3.d0
	end do
	w(N)=dx/3.d0
		
	hasil=0.d0
	do i=0,N
		x=xmin+i*dx
		hasil=hasil+w(i)*f(x)
	end do
	
	write(*,*) "Hasil integrasi =",hasil
	
	stop
	
	
	contains 
	
	FUNCTION f(x) RESULT(z)
	IMPLICIT NONE
	REAL*8, INTENT(IN) :: x
	REAL*8 :: z, pi
	pi=2.d0*asin(1.d0)
	!z=x**2*sin(pi*x)  	! coba 1
	z=x**2				! coba 2
	RETURN
	END FUNCTION
	
	end program