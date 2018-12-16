		program Heat_Transfer_Equation
!------------------------------------------------------------------------------------
! Code ini dibuat untuk menyelesaikan PD Parabolik 
! dengan contoh Pers, Perpindahan Panas 1 dimensi:  
!         (d^2/dx^2 - (1/Gamma) d/dt) Psi(x,t) = - 4 pi rho(x,t)
! pada obyek batang logam dengan panjang L=0.05 meter,
! dengan syarat-syarat batas:
! suhu di ujung kiri batang (tetap) = 100 derajat C: Psi(0,t)=100
! suhu di ujung kanan batang (tetap) = 0 derajat C: Psi(0,t)=0,
! dan syarat awal bahwa suhu batang mula-mula homogen 
! dengan suhu 25 derajat C, artinya Psi(x,0)=25.
!
! Secara fisis: Gamma = K/(rho_m*C)
! di mana rho_m = massa jenis batang, C = kapasitas panas
! dan K = konduktivitas panas.
! Sebagai contoh diberikan:
! K=54 watt/(meter*Kelvin), rho_m=7800 kg/m^3, C=490 joule/(kg*Kelvin).
!
! Untuk soal ini rho(x,t)=0.
!
! Akan dicari nilai-nilai Psi(x,t) pada 0<x<L, 0<t<t_max (misal tmax=10 detik).
!
! NOTE: metoda ini akan memberikan hasil yang salah jika lamnda = Gamma*ht/hx**2 > 1.
!------------------------------------------------------------------------------------
		implicit none
		integer :: i,j
		integer, parameter :: Nx=20, Nt=20 ! sesuai perbandingan A dan B
		real*8, parameter :: pi=2.d0*asin(1.d0)
		real*8 :: K,C,rho_m,Gamma,hx,ht,L,tmax,lambda
		real*8 :: x(0:Nx),t(0:Nt),rho(0:Nx,0:Nt)
		real*8 :: Psi(0:Nx,0:Nt)		
		
		L=0.05d0
		K=54.d0
		C=490.d0
		rho_m=7800.d0
		Gamma=K/(rho_m*C)	
		
		tmax=10.d0
			
		hx=L/real(Nx,8)
		ht=tmax/real(Nt,8)
		
		lambda=Gamma*ht/hx**2
		
		if (lambda.ge.1.d0) then			
			write(*,*) "lambda =",lambda
			write(*,*) "Perhitungan tidak jadi dilakukan karena nilai lambda > 1."
			write(*,*) "Hasil perhitungan akan salah jika dibiarkan lambda > 1."
			write(*,*) "Ubah dulu Nt dan/atau Nx supaya lambda < 1, baru lanjutkan."
			STOP
		end if
		
		x(0)=0.d0
		do i=1,Nx-1
			x(i)=real(i,8)*hx
		end do
		x(Nx)=L
		
		t(0)=0.d0
		do j=1,Nt-1
			t(j)=real(j,8)*ht
		end do
		t(Nt)=tmax
		
		do i=0,Nx
			do j=0,Nt
				rho(i,j)=0.d0
			end do
		end do
		
! Syarat-syarat batas:
		Psi(0,:)=100.d0
		Psi(Nx,:)=0.d0
		
! Syarat awal:
		do i=1,Nx-1		
			Psi(i,0)=25.d0		
		end do
		
! Perhitungan:
		
		do j=0,Nt-1
		do i=1,Nx-1
			Psi(i,j+1)=4.d0*Gamma*ht*pi*rho(i,j)+Psi(i,j)
			Psi(i,j+1)= Psi(i,j+1)+lambda*(Psi(i+1,j)-2.d0*Psi(i,j)+Psi(i-1,j))
		end do
		end do
				
! Tulis ke file dengan format agar bisa diplot 3D dengan gnuplot		
		open(unit=10, file="hasil_heat_transfer.dat", status="unknown")
			do i=0,Nx
				do j=0,Nt
					write(10,*) x(i),t(j),Psi(i,j)
				end do
			end do
		close (10)
		
	
		write(*,*) "lambda =",lambda
		
		write(*,*) "SELESAI"	
			
		
		end program