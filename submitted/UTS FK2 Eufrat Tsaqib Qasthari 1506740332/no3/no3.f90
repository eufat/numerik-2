program no3
	implicit none
	integer, parameter :: N=16,M=1000
	integer :: i,j,k,Np
	character*50 :: datafile,outputfile
	real*8 :: xd(N),f(N)
	real*8 :: x(M),p(M),dx,sum,prod,II(M,N)

	datafile="data_hamburan.txt"
	outputfile="hasil_data_hamburan.txt"
	Np=16


! Baca data
	open(unit=10, file=datafile, status="old")
	do i=1,N
		read(10,*) xd(i),f(i)
	end do

!   Definisikan titik-titik pengisi interpolasi
	x(1)=xd(1)
	x(M)=xd(N)
	dx=(x(M)-x(1))/real(M-1,8)
	do i=2,M-1
		x(i)=x(i-1)+dx
	end do

	open(unit=20, file=outputfile, status="unknown")

!	Perhitungan p(x)
	do k=1,M
		sum=0.d0
		do i=1,N
			prod=1.d0
			do j=1,N
				if (j==i) then
					cycle
				else
					prod=prod*(x(k)-xd(j))/(xd(i)-xd(j))
				end if
			end do	!j
			II(k,i)=prod
			sum=sum+II(k,i)*f(i)
		end do !i
		p(k)=sum
		write(20,*) x(k),p(k)
	end do



	stop
	end program