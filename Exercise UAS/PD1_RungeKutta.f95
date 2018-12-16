	program PD1_Runge_Kutta
	implicit none
!========================================================================
! Ditulis oleh: M. Aziz Majidi
!------------------------------------------------------------------------
! Problem:
! Perubahan muatan yang tersimpan pada kapasitor pada sebuah rangkaian 
! listrik seri RC arus bolak-balik memenuhi persamaan differensial orde 1:
! 	E(t) = R(dq/dt)+q/C
! di mana:
! E(t)= (100.d0)*sin(120.d0*pi*t) volts, R = 10 ohms, C=10^-3 farads, dan
! q=0 pada t=0.
! PD ini dapat ditulis menjadi:
! dq/dt = - q/(R*C) + E(t)/R .
! Jika variabel-variabelnya diganti menjadi: t-->x dan q-->y, maka PD tsb
! dapat ditulis kembali sebagai:
! dy/dx = f(x,y), dengan f(x,y)=-y/(R*C)+(100.d0/R)*sin(120.d0*pi*x),
! dengan syarat awal y0=0 pada x0=0. 
!
!==========================================================================

	integer :: i,N
	real*8 :: x0,y0,x,y,xmin,xmax,h,f0,f1,f2,f3
	real*8, external :: f
	
	x0=0.d0 ! syarat awal
	y0=0.d0
	
	xmin=0.d0  ! rentang daerah x yang akan dihitung y(x) nya.
	xmax=0.1d0 !
	N=100
	h=(xmax-xmin)/real(N,8)
	
	open(unit=10,file="PD1_RK.dat",status="unknown")
	
	do i=0,N
	
	x=xmin+real(i,8)*h
	
	if (i==0) then
		y=y0
		goto 100
	else
		f0=f(x0,y0)
		f1=f(x0+0.5d0*h,y0+0.5d0*h*f0)
		f2=f(x0+0.5d0*h,y0+0.5d0*h*f1)
		f3=f(x0+h,y0+h*f2)
	end if
	
	y=y0+h*(f0+2.0d0*f1+2.0d0*f2+f3)/6.0d0
	
100	write(10,*) x,"                      ",y
	x0=x
	y0=y 	
	
	end do
	
	close(10)
	
	write(*,*) "Perhitungan selesai." 
	write(*,*) "Data disimpan pada file PD1_RK.dat ."
	
	end program
	
	function f(x,y)
	implicit none
	real*8 :: x,y,f,R,C,pi
	R=10.d0 ! ohms
	C=1.d-3 ! farads
	pi=2.0d0*asin(1.0d0)
	f=-y/(R*C)+(100.d0/R)*sin(120.d0*pi*x)
	return 
	end