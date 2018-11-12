 	program findiff_gauss
	implicit none
	
	real*8, allocatable :: A(:), B(:), C(:), X(:), Y(:,:)
	real*8 :: h, alpha, gamma, lambda, beta, kesrel
	integer :: N,i,k, itermax
	real*8, parameter :: eps = 1.d-12
	
	! Jumlah titik 
	N = 100				!###### GANTI WEY ######!
	itermax = 10000		!###### GANTI WEY ######!
	
	! Alokasi matriks
	allocate(A(1:N-1))
	allocate(B(1:N-1))
	allocate(C(1:N-1))
	allocate(X(0:N))
	allocate(Y(0:N,2))
	
	! Rentang nilai X
	X(0) = 0.d0		!xmin	!###### GANTI WEY ######!
	X(N) = 1.d0		!xmax	!###### GANTI WEY ######!
	
	h = (X(N) - X(0))/real(N,8)
	
	do i = 1, N-1
		X(i) = X(0) + i*h
	end do
	
	! Syarat batas
	Y(0,:) = 1.d0	!ymin	!###### GANTI WEY ######!
	Y(N,:) = 0.d0	!ymax	!###### GANTI WEY ######!
	
	! Tebakan awal
	do i = 1, N-1
		y(i,:) = -X(i)+1.d0	 ! tebakan awal	!###### GANTI WEY ######!
	end do
	
	! Koefisien a, b, c >>  y''+ cy' + by = a
	do i = 1, N-1
		A(i) = 2.d0*X(i)		!###### GANTI WEY ######!
		B(i) = 1.d0				!###### GANTI WEY ######!
		C(i) = X(i)				!###### GANTI WEY ######!
	end do
	
	! Iterasi
	do i = 1, N-1
		alpha  = -A(i)*h**2
		beta   = 1-(C(i)*h/2)
		gamma  = 1+(C(i)*h/2)
		lambda = 1/(2-B(i)*h**2)
		do k = 1, itermax
			y(i,2) =  lambda*(alpha + beta*y(i-1,2) + gamma*y(i+1,1) )
			kesrel = ABS (1-(y(i,1)/y(i,2)))
			if (kesrel <= eps) exit
			y(i,1) = y(i,2)
		end do
		
	end do
	
	! Tulis ke file
	open(unit=12, file='hasil_findiff_gauss.txt', status = 'replace', action='write')
	
	do i=0,N
		write(12,*) X(i), "                 ",Y(i,2)
	end do
	
	close(12)
	
	write(*,*) "Perhitungan Selesai"
	
	deallocate(A)
	deallocate(B)
	deallocate(C)
	deallocate(X)
	deallocate(Y)
	
	stop
	end program