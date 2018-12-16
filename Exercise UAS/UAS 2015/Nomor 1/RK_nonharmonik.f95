	program PDB2_Runge_Kutta
	implicit none
	integer :: i,N
	real*8 :: x0,y0,u0,x,y,u,xmin,xmax,h,f0,f1,u1,f2,u2,f3,u3
	real*8 :: pi
	character*50 :: outputfile1, outputfile2
	real*8, external :: f
	
	x0=0.d0 ! syarat awal
	y0=0.5d0
	u0=0.d0
	
	pi=2.d0*asin(1.d0)
	
	xmin=0.d0  ! rentang daerah x yang akan dihitung y(x) nya.
	xmax=8.d0*pi !
	N=500
	h=(xmax-xmin)/real(N,8)
	
	outputfile1 = "Nonharmonik_xy.txt"
	outputfile2 = "Nonharmonik_xu.txt"
	open(unit=10,file=outputfile1, status="unknown")
	open(unit=20, file=outputfile2, status="unknown")
	
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
	write(*,*) "Data disimpan pada file ",outputfile1,"dan ",outputfile2	
	end program
	
	function f(x,y,u)
	implicit none
	real*8 :: x,y,u,f
	f = (-y) - (0.3*((y**2)-1))*u
	write(*,*) y
	return 
	end