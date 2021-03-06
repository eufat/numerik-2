	program main
	implicit none
	integer :: N,i,idx_min,idx_max
	real*8, allocatable :: xd(:), fd(:)
	real*8 :: hasil,h
	real*8, external :: trapezoid_data !###### GANTI WEY ######!
	character*50 :: inputfile

! Proses pembacaan data masukan
	inputfile = "data_integral.txt" !###### GANTI WEY ######!
	
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N	
	allocate(xd(N))
	allocate(fd(N))
		
	read(10,*)
	do i=1,N
		read(10,*) xd(i), fd(i)
	end do
	close(10)
	
	do i=1,N
		write(*,*)i,"  ", xd(i)," ",fd(i)
	end do
	
	write(*,*) "idx_min"
	read(*,*)   idx_min
	write(*,*) "idx_max"
	read(*,*)	idx_max
	h=xd(2)-xd(1)
	
	hasil = trapezoid_data(fd,idx_min,idx_max,h,N)
	
	!Tulis hasil integrasi
	write(*,*) "Hasil integrasi =",hasil
	
	deallocate(xd)
	deallocate(fd)
	
	end program