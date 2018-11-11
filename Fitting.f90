		program Fitting
		implicit none

		integer, parameter :: N=10,M=5
		real*8 :: x(N),f(N),C(M+1,M+1),A(M+1),B(M+1)
		integer :: i,j,k,Np
		character*50 :: datafile,outputfile
		real*8 :: dummy,xmin,xmax,xx,dx,p


		datafile="data_medan_listrik.dat"
		outputfile="hasil_fitting_medan_listrik.dat"
		Np=1000


		! Baca data
		open(unit=10, file=datafile, status="old")
		do i=1,N
			read(10,*) x(i), f(i)
		end do

		! Menghitung koefisien-koefisien C(k,j)
		do k=0,M
			do j=0,M
				dummy=0.d0
				do i=1,N
					dummy=dummy+X(i)**(j+k)
				end do
				C(k+1,j+1)=dummy
			end do
		end do

		! Menghitung koefisien-koefisien B(k)
		do k=0,M
			dummy=0.d0
			do i=1,N
				dummy=dummy+f(i)*x(i)**k
			end do
			B(k+1)=dummy
		end do

		call LUdcmp(M+1,C,B,A)

		! Konstruksi polinomial p(x)
		open(unit=20, file=outputfile, status="unknown")

		xmin=X(1)
		xmax=X(N)
		dx=(xmax-xmin)/(Np-1)
		do i=1,Np
			xx=xmin+(i-1)*dx
			p=0.d0
			do j=1,M+1
				p=p+A(j)*xx**(j-1)
			end do
			write(20,*) xx, p
		end do


		write(*,*) "Perhitungan fitting selesai. Hasil disimpan dalam file ",outputfile

		stop

		end program
