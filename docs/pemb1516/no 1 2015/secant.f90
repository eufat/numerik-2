	program secant

	implicit none
	real*8, parameter :: tol=1.d-5
	integer :: n
	real*8 :: a,b,c,x1,x2,kesrel
	
	
	do
 		write(*,*) 'Masukkan batas kiri, batas kanan:'
 		read (*,*) a,b
 		if (f(a)*f(b) < 0.d0) then
 			exit
		else
			write(*,*) 'Batas kiri-kanan tidak mengapit akar fungsi.'
			write(*,*) 'Coba lagi dengan nilai-nilai yang lain.'
		end if
	end do


	n=0
	
	do 
		x1=(b*f(a)-a*f(b))/(f(a)-f(b))
		if (x1 >= 3) then
		x2=((b-2)*f(a-1)-((b-1)*f(a-2)))/(f(a-1)-f(a-2))
		x2 = x1
		end if
 		kesrel=abs((a-x1)/x1)
 		n=n+1
 		if (kesrel < tol) exit
 		a=x1
	end do

	write(*,*) 'Pencarian akar konvergen pada langkah ke-', n
	write(*,*) 'Akar    = ',x1
	write(*,*) 'f(akar) = ',f(x1) 
	write(*,*) 'Kesalahan relatif =',kesrel

	stop

	contains

	function f(x) result(y)

	implicit none
	real*8, intent(in) :: x
	real*8 :: y

	y=cos(x)-x

	return

	end function f

	end program secant
