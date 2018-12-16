	program simpson
!--------------------------------------------------
! Keterangan:
! Program untuk menghitung g(x) =integral f(x')dx' 
! dengan batas-batas dari x'=xmin dan x='x.
! Hasil integrasi disimpan dalam file data berisi 2 
! kolom: x, g(x) 
!--------------------------------------------------
	implicit none
	integer :: i
	integer, parameter :: N=100
	real*8 x,xmin,xmax,dx,hasil
	
	xmin=0.d0
	xmax=2.d0
	
	dx=(xmax-xmin)/real(N,8)
	
	open(unit=10,file="Hasil_integrasi_simpson.txt",status="unknown")
	
	write(*,*) xmin, 0.d0	
		
	hasil=0.d0
	do i=2,N,2
		x=xmin+i*dx
		hasil=hasil+(dx/3.d0)*f(x-2.d0*dx)+(4.d0*dx/3.d0)*f(x-dx)+(dx/3.d0)*f(x)
		write(*,*) x, hasil
		write(10,*) x, hasil
	end do
	
	
	stop
	
	
	contains 
	
	FUNCTION f(x) RESULT(z)
	IMPLICIT NONE
	REAL*8, INTENT(IN) :: x
	REAL*8 :: z, pi
	pi=2.d0*asin(1.d0)	! coba 1
	!z=x**2*sin(pi*x)	! coba 2
	z=x**2
	RETURN
	END FUNCTION
	
	end program