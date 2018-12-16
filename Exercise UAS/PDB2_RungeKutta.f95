	program PDB2_Runge_Kutta
	implicit none
!=====================================================================================
! Ditulis oleh: Feby Nirwana
!-------------------------------------------------------------------------------------
! Problem:
! Perubahan muatan yang tersimpan pada kapasitor pada sebuah rangkaian 
! listrik seri RC arus bolak-balik memenuhi persamaan differensial orde 2:
! 	E(t) = R(dq/dt)+ q/C + L(d^2q/dt^2)
! di mana:
! E(t)= (120.d0)*sin(377*t) volts, R = 1000 ohms, C = 2*10^-6 farads,
! L = 3.5 Henry dan q=0 pada t=0.
! PD ini dapat ditulis menjadi:
! d^2q/dt^2 = E(t)/L - R*I/L - q/(L*C) .
! Jika variabel-variabelnya diganti menjadi: t-->x, q-->y, I-->u maka PDB2
! tersebut dapat ditulis kembali sebagai:
! d^2y/dx^2 = f(x,y,u), dengan f(x,y,u)=((120.d0)*sin(377*x))/L - (R*u)/L - y/(L*C),
! dengan syarat awal y0=0 dan u0=0 pada x0=0. 
!
!=====================================================================================

	integer :: i,N
	real*8 :: x0,y0,u0,x,y,u,xmin,xmax,h,f0,f1,u1,f2,u2,f3,u3
	real*8, external :: f
	
	x0=0.d0 ! syarat awal
	y0=0.d0
	u0=0.d0
	
	xmin=0.d0  ! rentang daerah x yang akan dihitung y(x) nya.
	xmax=0.1d0 !
	N=500
	h=(xmax-xmin)/real(N,8)
	
	open(unit=10,file="PD2_RK_xy.txt",status="unknown")
	open(unit=20, file="PD2_RK_xu.txt", status="unknown")
	
	do i=0,N
	
	x=xmin+real(i,8)*h
	
	if (i==0) then
		y=y0
		u=u0
		goto 100
	else
		f0 = f(x0,y0, u0)
		u1 = u0+0.5*h*f0
			
		f1 = f(x0+0.5*h, y0+0.5*h*u0,u1)
		u2 = u0+0.5*h*f1

		f2 = f(x0+0.5*h, y0+0.5*h*u1,u2)
		u3 = u0+h*f2
			
		f3 = f(x0+h, y0+h*u2, u3)			
		
	end if
	u  = u0+h*(f0+2.d0*f1+2.d0*f2+f3)/6.d0
	y  = y0+h*(u0+2.d0*u1+2.d0*u2+u3)/6.d0
	
100	write(10,*) x,"                       ",y
	write(20,*) x,"                       ",u
	x0 = x
	y0 = y
	u0 = u
	
	end do
	
	close(10)
	close(20)
	
	write(*,*) "Perhitungan selesai." 
	write(*,*) "Data disimpan pada file PD2_RK_xy.txt dan PD2_RK_xu.txt ."
	
	end program
	
	function f(x,y,u)
	implicit none
	real*8 :: x,y,f,R,C,L,u
	R=1000.d0 ! ohms
	L=3.5	! henry
	C=2.d-6 ! farads
	f = ((120.d0)*sin(377*x))/L - (R*u)/L - y/(L*C)
	return 
	end