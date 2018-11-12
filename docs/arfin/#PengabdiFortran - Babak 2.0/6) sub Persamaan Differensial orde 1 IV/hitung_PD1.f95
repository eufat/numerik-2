	program hitung_PD1
	implicit none
	integer :: N
	real*8 :: x0, y0, x, y, xmin, xmax
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0 		!###### GANTI WEY ######!
	y0 = 0.d0		!###### GANTI WEY ######!
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0		!###### GANTI WEY ######!
	xmax = 0.1d0	!###### GANTI WEY ######!
	! Jumlah titik data
	N = 1000		!###### GANTI WEY ######!
	
	!call rungekutta4_PD1(f,x0,y0,xmin,xmax,N)
	!call euler(f,x0,y0,xmin,xmax,N)
	!call euler_m(f,x0,y0,xmin,xmax,N)
	call euler_i(f,x0,y0,xmin,xmax,N)

	
	stop
	end program 
	
	real*8 function f(x,y) 
	implicit none
	real*8 :: x,y,R,C,pi
	R = 10.d0
	C = 1.d-3
	pi = 2*asin(1.0)
	f = -y/(R*C)+(100/R)*sin(120*pi*x) !###### GANTI WEY ######!
	return
	end