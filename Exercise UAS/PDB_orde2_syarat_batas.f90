	program PDB_orde2_syarat_batas
	implicit none
!----------------------------------------------------------------	
! 	Code untuk menyelesaikan PDB orde 2 dengan syarat batas.
!	Rumus umum: y"+c(x)y'+b(x)y=a(x)
!	Contoh soal PDB diambil dari (sekedar contoh saja):	
!	https://www.stewartcalculus.com/data/CALCULUS%20Concepts%20and%20Contexts/upfiles/3c3-2ndOrderLinearEqns_Stu.pdf
!	y"+4y'+13y=0,	syarat batas: y(0)=2,	y(pi/2)=1
!----------------------------------------------------------------	
	integer, parameter :: N=200,Niter=100000
	integer :: i,iter
	real*8 :: pi,h,error,toleransi,mixing
	real*8 :: x(0:N),y(0:N),yt(0:N),a(0:N),b(0:N),c(0:N)
	
	
	pi=3.14159265359
	toleransi=1.d-5
	mixing=0.1d0
	
	! Buka file output
	open(unit=10, file="Hasil_PDB2_SB.dat", status="unknown")
	
	
	! Tabulasi harga x
	x(0)=0.d0
	x(N)=pi/2.d0
	h=(x(N)-x(0))/real(N,8)
	do i=1,N-1 
		x(i)=x(0)+real(i,8)*h
	end do
	
	! Identitas PDB
	do i=0,N
		c(i)=4.d0
		b(i)=13.d0
		a(i)=0.d0
	end do
	
	! Syarat batas
	y(0)=2.d0
	y(N)=1.d0
	
	yt(0)=y(0)
	yt(N)=y(N)
	
	! Tebakan awal untuk y 
	! Untuk contoh saja, kita pilih persamaan garis lurus yang melewati titik-titik batas.
	do i=1,N-1
		yt(i)=(yt(N)-yt(0))/(x(N)-x(0))*x(i) + yt(0)
	end do
	
	! Hitung y
	do iter=1,Niter

		do i=1,N-1
			y(i)=a(i)*h**2 - (1.d0-0.5d0*c(i)*h)*yt(i-1) - (1.d0+0.5d0*c(i)*h)*yt(i+1)
			y(i)=-y(i)/(2.d0-b(i)*h**2)
		end do

	
	! Cek kesalahan
	error=0.d0
	do i=1,N-1
		error=error+abs(1.d0-yt(i)/y(i))
	end do
	error=error/real(N-1,8)
	
	write(*,*) "iter =",iter,"error =",error
	
	if (error.lt.toleransi) exit		 
	
	yt(:)=(1.d0-mixing)*yt(:)+mixing*y(:)
	
	end do ! iter
	
	! Tulis ke file output
	do i=0,N
		write(10,*) x(i), y(i) 
	end do
	
	close(10)
	
	write(*,*) "Perhitungan selesai."
	
	
	end program