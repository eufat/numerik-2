	real*8 function rho(x,y) result(z)
	implicit none
	real*8, intent(in) :: x,y
	real*8 :: func
	real*8, parameter :: pi=2.d0*asin(1.d0)
		
		func = 			!###### GANTI WEY ######!
		
		z = -func/(4.d0*pi)
		
	end function
	
	program PDP_Parabolik
	implicit none
	real*8, parameter :: pi = 2.d0*asin(1.d0)
	real*8, parameter :: epsilon = 1.d-5
	real*8, external :: rho
	real*8, allocatable :: x(:), b(:), t(:), psi(:,:), margin(:)
	real*8 :: hx, ht, koef_ddt, c, stable, alpha, beta, gamma, lambda tFinal, marginmax
	integer :: i, j, Nx, Nt, Nt_write
	
	! ingat : - (1/gamma) d/dt
	! 		  - koef_ddt d/dt
	koef_ddt = 			!###### GANTI WEY ######!
	
	gamma = (1/koef_ddt)
	
	write(*,*) "Masukkan nilai t maksimal : "
	read(*,*) tFinal
	
	do
		write(*,*) "Masukkan nilai Nx : "
		read(*,*) Nx
		write(*,*) "Masukkan nilai Nt : "
		read(*,*) Nt
		Nt_write = Nt
	
		! alokasi memori
		allocate(x(0:Nx))
		allocate(t(0:Nt))
		allocate(psi(0:Nx, 0:Nt))
		allocate(margin(0:Nx))

		
		x(0)  = 0.d0		!xmin	!###### GANTI WEY ######!
		x(Nx) = 1.d0		!xmax	!###### GANTI WEY ######!
		t(0)  = 0.d0		!tmin	!###### GANTI WEY ######!
		t(Nt) = tFinal		
		
		hx = (x(Nx) - x(0))/Nx
		ht = (t(Nt) - t(0))/Nt
		
		lambda = (gamma*ht/hx/hx)
		write(*,*) "Lambda = ", lambda
		
		if (lambda<=0.5d0) then
			write(*,*) "Syarat stabilitas telah terpenuhi. Sedang mencari solusi PDP parabolik"
			exit
		end if
		
		write(*,*) "Syarat stabilitas belum terpenuhi"

	deallocate(x)
	deallocate(t)
	deallocate(psi)
	deallocate(margin)

		
	end do

	! Mengisi x dan t
	do i = 1, Nx-1
		x(i) = x(i-1)+hx
	end do
	do i = 1, Nt-1
		t(i) = t(i-1)+ht
	end do
	
	! Syarat batas
	psi(:,:) = 0.d0
	psi(0,:) = 		!nilai psi di batas 0 !###### GANTI WEY ######!
	psi(Nx,:) =		!nilai psi di batas max !###### GANTI WEY ######!	
	do i=1,Nx-1
		psi(i,0) = 	!nilai psi diantara batas !###### GANTI WEY ######!
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