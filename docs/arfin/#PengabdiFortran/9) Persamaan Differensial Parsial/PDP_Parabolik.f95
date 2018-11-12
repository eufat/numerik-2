	real*8 function rho(x,y) result(z)
	implicit none
	real*8, intent(in) :: x,y
	real*8 :: func
	real*8, parameter :: pi=2.d0*asin(1.d0)
		
		func = 	0		!###### GANTI WEY ######!
		
		z = -func/(4.d0*pi)
		
	end function
	
	program PDP_Parabolik
	implicit none
	real*8, parameter :: pi = 2.d0*asin(1.d0)
	real*8, parameter :: epsilon = 1.d-5
	real*8, external :: rho
	real*8, allocatable :: x(:), b(:), t(:), psi(:,:), margin(:)
	real*8 :: hx, ht, koef_ddt, c, stable, alpha, beta, gamma,tFinal, marginmax, thres, x0, xN, t0
	integer :: i, j, Nx, Nt, Nt_write
	
	! ingat : - (1/gamma) d/dt
	! 		  - koef_ddt d/dt
	koef_ddt = 		903*2702/933	!###### GANTI WEY ######!
	
	gamma = (1/koef_ddt)

	! Nilai batas
	x0 = 0		!xmin	!###### GANTI WEY ######!
	xN = 1.d0	!xmax	!###### GANTI WEY ######!
	t0 = 0		!t_awal	!###### GANTI WEY ######!
	
	write(*,*) "Masukkan nilai t maksimal : "
	read(*,*) tFinal
	
	
	! Jumlah titik
123	write(*,*) "Masukkan nilai Nx : "
	read(*,*) Nx
	hx = (xN - x0)/Nx

	thres = 2*(tFinal-t0)*gamma/hx/hx
	
	write(*,*) "Untuk Nx=",Nx," ambil Nt >=",thres
	write(*,*) "Masukkan nilai Nt : "
	read(*,*) Nt

	! Interval (hx =/= ht)
	ht = (tFinal - t0)/Nt 
	
	stable = (gamma*ht/hx/hx)
	write(*,*) "Stabilizer = ", stable
	
	if (stable<=0.5d0) then
		write(*,*) "Sistem stabil. Proses perhitungan sedang berlangsung......."
	else
		write(*,*) "Sistem belum stabil! Silakan sesuaikan Nx dan Nt kembali~"
		write(*,*)
		goto 123
	end if
	
	! alokasi memori
	allocate(x(0:Nx))
	allocate(t(0:Nt))
	allocate(psi(0:Nx, 0:Nt))
	allocate(margin(0:Nx))
	x(0)  = x0
	x(Nx) = xN
	t(0)  = t0
	t(Nt) = tFinal
	
	Nt_write = Nt


	! Mengisi x dan t
	do i = 1, Nx-1
		x(i) = x(i-1)+hx
	end do
	do i = 1, Nt-1
		t(i) = t(i-1)+ht
	end do
	
	! Syarat batas
	psi(:,:) = 0.d0
	psi(0,:) = 	0	!nilai psi di batas 0 !###### GANTI WEY ######!
	psi(Nx,:) =		110!nilai psi di batas max !###### GANTI WEY ######!	
	do i=1,Nx-1
		psi(i,0) = 	27!nilai psi diantara batas !###### GANTI WEY ######!
	end do
	
	
	! Perhitungan
	do j=0, Nt-1
		do i=1,Nx-1
			alpha    = 4*gamma*ht*pi*rho(x(i), t(j))
			beta     = gamma*ht/hx/hx
			psi(i,j+1) = alpha + psi(i,j) + beta*(psi(i+1,j)-2*psi(i,j)+psi(i-1,j)) 	
		end do
		
		margin(:) = abs(psi(:,j+1) - psi(:,j))
		marginmax = maxval(margin)
		
		if(marginmax<epsilon) then
			write(*,*) "Telah tercapai keaadan stasioner pada t = ", t(j+1)
			Nt_write = j
			exit
		end if
	end do
	
	if (Nt_write == j) then
		write(*,*)
	else 
		write(*,*) "Hasil yang diperoleh belum mencapai keadaan stasioner! &
		&Silakan hitung lagi (bila diperlukan) dengan meningkatkan nilai t!"
	end if
	
	open(unit=10, file = 'PDP-Parabolik.dat', status='unknown')
	do i=0,Nx
		do j=0,Nt_write
			write(10,*) x(i),"			",t(j),"			",psi(i,j)
		end do
		write(10,*)
	end do
	close(10)
	
	deallocate(x)
	deallocate(t)
	deallocate(psi)
	deallocate(margin)

	
	end program