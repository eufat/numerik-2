		program Poisson_Equation
!---------------------------------------------------------------
! Code ini dibuat untuk menyelesaikan PD Eliptik (Pers, Poisson)
! 2 dimensi:
!         (d^2/dx^2 + d^2/dy^2) Psi(x,y) = - 4 pi rho(x,y)
! pada obyek berbentuk persegi panjang berukuran Luas = AB,
! dengan syarat-syarat batas:
! Psi(0,y)=Psi(A,y)=0, dan Psi(x,0)=P(x,B)=2.
! Untuk contoh, misal A=2 unit, B=3 unit,
! dan rho(x,y)=c(x-A/2)(y-B/2) dengan c=1.
! Akan dicari nilai-nilai Psi(x,y) pada 0<x<A, 0<y<B.
!---------------------------------------------------------------
		implicit none
		integer :: i,j,iter,Niter
		integer, parameter :: Nx=40, Ny=60 ! sesuai perbandingan A dan B
		real*8, parameter :: pi=2.d0*asin(1.d0)
		real*8 :: A,B,c,h,error,toleransi,mixing
		real*8 :: x(0:Nx),y(0:Ny),rho(0:Nx,0:Ny)
		real*8 :: Psi(0:Nx,0:Ny),Psit(0:Nx,0:Ny)


		A=2.d0
		B=3.d0
		c=1.d0

		toleransi=1.d-3
		Niter=5000
		mixing=0.5d0

		h=A/real(Nx,8)

		x(0)=0.d0
		do i=1,Nx-1
			x(i)=real(i,8)*h
		end do
		x(Nx)=A

		y(0)=0.d0
		do j=1,Ny-1
			y(j)=real(j,8)*h
		end do
		y(Ny)=B

		do i=0,Nx
			do j=0,Ny
				rho(i,j)=c*(x(i)-0.5d0*A)*(y(j)-0.5d0*B)
			end do
		end do

! Syarat-syarat batas:
		Psi(0,:)=0.d0
		Psi(Nx,:)=0.d0
		Psi(:,0)=2.d0
		Psi(:,Ny)=2.d0

! Psi(x,y) tebakan (untuk contoh saja Psi(x,y)=1.d0
		Psit(:,:)=1.d0
		Psit(0,:)=0.d0
		Psit(Nx,:)=0.d0
		Psit(:,0)=2.d0
		Psit(:,Ny)=2.d0

! Perhitungan dengan iterasi:
		do iter=1,Niter

		do i=1,Nx-1
		do j=1,Ny-1
			Psi(i,j)=h**2*pi*rho(i,j)+0.25d0*(Psit(i+1,j)+Psit(i-1,j)+Psit(i,j+1)+Psit(i,j-1))
		end do
		end do

! Cek kesalahan:
		error=0.d0
		do i=1,Nx-1
		do j=1,Ny-1
			error=error+abs(1.d0-Psit(i,j)/Psi(i,j))
		end do
		end do
		error=error/real((Nx-1)*(Ny-1),8)

		write(*,*) "Iter =",iter,"Error =",error

		if (error.lt.toleransi) exit

		Psit(:,:)=(1.d0-mixing)*Psit(:,:)+mixing*Psi(:,:)

		end do ! iter


! Tulis ke file dengan format agar bisa diplot 3D dengan gnuplot
		open(unit=10, file="hasil_Poisson.dat", status="unknown")
			do i=0,Nx
				do j=0,Ny
					write(10,*) x(i),y(j),Psi(i,j)
				end do
			end do
		close (10)

		write(*,*) "SELESAI"


		end program