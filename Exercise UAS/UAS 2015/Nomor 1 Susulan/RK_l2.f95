	program PDB2_Runge_Kutta
	implicit none
	integer :: i,N
	real*8 :: x0,y0,u0,x,y,u,xmin,xmax,h,f0,f1,u1,f2,u2,f3,u3
	character*50 :: outputfile1, outputfile2
	real*8, external :: f
	
	x0=0.d0 ! syarat awal
	y0=0.d0
	u0=0.d0
	
	xmin=0.d0  ! rentang daerah x yang akan dihitung y(x) nya.
	xmax=10.d0 !
	N=100
	h=(xmax-xmin)/real(N,8)
	
	
	outputfile1 = "l2_xy.txt"
	outputfile2 = "l2_xu.txt"
	open(unit=10,file=outputfile1, status="unknown")
	open(unit=20, file=outputfile2, status="unknown")
	
	
	
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
	real*8 :: Vo,lamda,E,m,h,Vr,z,l
	l = 2
	Vo = 600
	lamda = 1.5
	E = 100
	m = 938.27
	h = 197.33
	Vr = Vo*(exp(-lamda*x)/x)
	z = ((2*m*(E-Vr))/h**2) - ((l*(l+l))/x**2)
	f = (-2/x)*u - (z*y)
	return 
	end function