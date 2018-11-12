	! Fungsi
	real*8 function rho(x,y) result(z)
	implicit none
	real*8, intent(in) :: x,y
	real*8 :: func
	real*8, parameter :: pi=2.d0*asin(1.d0)
		
		! Tulis Fungsi disini
		func = (4.d0*pi)
		
		z = -func/(4.d0*pi)
		
	end function
	
	program PDP_Eliptik
	implicit none
	real*8, parameter :: epsilon = 1.d-5
	real*8, external :: rho
	real*8, allocatable :: x(:), y(:), psi(:,:,:), error(:,:)
	real*8 :: h, C, errormax, pi
	integer :: i, j, k, Nx, Ny, itermax, no_output
	pi = 2.d0*asin(1.d0)
	no_output = 0
	! Jumlah titik
	write(*,*) "Masukkan nilai Nx : "
	read(*,*) Nx
	write(*,*) "Masukkan nilai Ny : "
	read(*,*) Ny
	! Iterasi maksimum (in case tidak kunjung converge)
	write(*,*) "Masukkan nilai iterasi maksimum : "
	read(*,*) itermax
	
	! alokasi memori
	allocate(x(0:Nx))
	allocate(y(0:Ny))
	allocate(psi(0:Nx, 0:Ny, 2))
	allocate(error(0:Nx, 0:Ny))
	
	

	! Nilai batas
	x(0)  = 0.d0
	x(Nx) = 1.d0
	y(0)  = 0.d0
	y(Ny) = 1.d0
	
	! Interval (hX == hY)
	h = (x(Nx) - x(0))/Nx
	
	! Mengisi x dan y
	do i = 1, Nx-1
		x(i) = x(i-1)+h
	end do
	do i = 1, Ny-1
		y(i) = y(i-1)+h
	end do
	
	! Syarat batas
	psi(:,:,:) = 0.d0
	do i=0,Nx
		psi(i,0,:)  = 1.d0
		psi(i,Ny,:) = 1.d0
	end do
	do i = 0,Ny
		psi(0,i,:)  = -1.d0
		psi(Nx,i,:) = -1.d0
	end do
	
	! Perhitungan
	do k = 1, itermax
		do i = 1, Nx-1
			do j = 1, Ny-1
				C = (h**2)*pi*rho(x(i), y(j))
				psi(i,j,2) = C + 0.25d0*(psi(i+1,j,1)+psi(i-1,j,1)+psi(i,j+1,1)+psi(i,j-1,1))	
			end do
		end do
		
		error(:,:) = abs(psi(:,:,1)-psi(:,:,2))
		errormax = maxval(error)
		
		if (errormax<epsilon) then
			write(*,*) "Konvergensi tercapai pada iterasi : ", k
			exit
		end if
		
		psi(:,:,1) = psi(:,:,2)
		
		if (k == itermax) then
			write(*,*) "Konvergensi tidak tercapai!"
			no_output = 1
		end if
	end do
	
	if (no_output==1) goto 123
	
	open(unit=10, file = 'hasil_PDP-Eliptik.txt', status='unknown')
	do i=0,Nx
		do j=0,Ny
			write(10,*) x(i),"			",y(j),"			",psi(i,j,2)
		end do
		write(10,*)
	end do
	close(10)
	
123	deallocate(x)
	deallocate(y)
	deallocate(psi)
	deallocate(error)
	
	end program