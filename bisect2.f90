	program bisection

	implicit none
	real*8, parameter :: toleransi=1.d-5
	integer :: langkah
	real*8 :: a,b,x1,x2,kesrel
	
	
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

	x1=(a+b)*0.5d0

	langkah=1
	
	do 

 		if (f(a)*f(x1) < 0.d0) then
  			b=x1
 		else
  			a=x1
 		end if 

 		x2=(a+b)*0.5d0

 		kesrel=abs((x1-x2)/x2)

 		langkah=langkah+1
 		
 		if (kesrel < toleransi) exit

 		x1=x2

	end do

	write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah
	write(*,*) 'Akar    = ',x2
	write(*,*) 'f(akar) = ',f(x2) 
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

	end program bisection
