	program hitung_PD2
	implicit none
	integer :: N
	real*8 :: x0, y0, u0, x, y, xmin, xmax
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0	!###### GANTI WEY ######!
	y0 = 0.d0	!###### GANTI WEY ######!
	u0 = 0.d0	!###### GANTI WEY ######!
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0		!###### GANTI WEY ######!
	xmax = 0.1d0	!###### GANTI WEY ######!
	! Jumlah titik data	
	N = 10000		!###### GANTI WEY ######!
	
	call rungekutta4_PD2(f,x0,y0,u0,xmin,xmax,N)
	!call euler_PD2(f,x0,y0,u0,xmin,xmax,N)

	
	stop
	end program 
	
	
	! Ganti fungsi disini
	real*8 function f(x,y,u)
	implicit none
	real*8 :: x,y,u
	
		f = -4.d2*u-2.d5*y+1.d3*cos(150.d0*x) !###### GANTI WEY ######!

	return
	end