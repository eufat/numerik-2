program newton
	implicit none
	real*8, parameter :: toleransi=1.d-5
	integer :: langkah
	real*8 :: guess,x1,x2,kesrel


	write(*,*) 'Masukkan perkiraan:'
	read (*,*) guess


	langkah=1

	do
		x2=guess-(f(guess)/df(guess))
		kesrel=abs((x1-x2)/x2)
		langkah=langkah+1

		if (kesrel < toleransi) exit
		x1=x2
	end do

	write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah
	write(*,*) 'Akar    = ', x2
	write(*,*) 'f(akar) = ', f(x2)
	write(*,*) 'Kesalahan relatif =', kesrel

	stop

	contains

	function f(x) result(y)
		implicit none
		real*8, intent(in) :: x
		real*8 :: y

		y=cos(x)-x

		return
	end function f

	function df(x) result(y)
		implicit none
		real*8, intent(in) :: x
		real*8 :: y

		y=-sin(x)-1

		return
	end function df
end program newton
