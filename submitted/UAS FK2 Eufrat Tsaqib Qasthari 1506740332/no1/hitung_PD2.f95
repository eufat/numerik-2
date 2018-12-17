	program hitung_PD2
	implicit none
	integer :: N
	real*8 :: x0, y0, u0, x, y, xmin, xmax
	real*8, external :: f

	! Syarat awal
	x0 = 0.d0
	y0 = 0.d0
	u0 = 0.d0

	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0
	xmax = 0.03
	! Jumlah titik data
	N = 1000

	call rungekutta4_PD2(f,x0,y0,u0,xmin,xmax,N)


	stop
	end program


	! Ganti fungsi disini
	real*8 function f(x,y,u)
	implicit none
	real*8 :: x,y,u
	real*8 :: E,R,L,C
	real :: pi = 3.1415927

	E =100*cos(2*pi*120*x)
	R = 2.5
	L = 0.0001
	C = 0.00008

	f = E/L - (y/(L*C)) -R*u/L

	return
	end