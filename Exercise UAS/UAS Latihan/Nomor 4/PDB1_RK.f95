	program PD1_Runge_Kutta
	implicit none

	integer :: i,N
	real*8 :: x0,y0,x,y,xmin,xmax,h,f0,f1,f2,f3
	real*8, external :: f
	character*50 :: outputfile
	
	x0=0.d0 ! syarat awal
	y0=1.d0
	
	xmin=0.d0  ! rentang daerah x yang akan dihitung y(x) nya.
	xmax=3.d0 !
	N=100
	h=(xmax-xmin)/real(N,8)
	
	outputfile = 'Hasil_PDB1_RK.txt'
	open(unit=10,file=outputfile,status="unknown")
	
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
	write(*,*) "Data disimpan pada ",outputfile
	
	end program
	
	function f(x,y)
	implicit none
	real*8 :: x,y,f
	f=(5*x)-(4*x*y)
	return 
	end