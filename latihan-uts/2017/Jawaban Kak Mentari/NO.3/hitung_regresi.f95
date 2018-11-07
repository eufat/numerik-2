	program hitung_regresi
	implicit none
	integer :: N,m,i
	real*8, allocatable :: xd(:), fd(:),a(:)
	
	character*50 :: inputfile

! Proses pembacaan data masukan
	inputfile = "data_UTS17.txt" !GANTI SESUAI NAMA DATA
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N	
	allocate(xd(N))
	allocate(fd(N))
		
	read(10,*)
	do i=1,N
		read(10,*) xd(i), fd(i)
	end do
	close(10)

! Menampilkan hasil pembacaan data masukan	
	write(*,*) "Hasil baca file input"
	do i=1,N
		write(*,*) xd(i)," ",fd(i)
	end do
	
! Menentukan orde polinomial 
	write(*,*)
	write(*,*) "Orde polinomial yang diinginkan : "
	read(*,*)	m
	allocate(a(m+1))

	call regresi(N,xd,fd,m,a)
	
	! Tampilkan isi a
	write(*,*)
	write(*,*) "Koefisien Polinomial:"
	do i=1,m+1
		write(*,*) "a(",i-1,")= ",a(i)
	end do

 
	deallocate(xd)
	deallocate(fd)
	stop
	
	end program 