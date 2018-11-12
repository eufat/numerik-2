	program hitung_interpolasi
	implicit none
	integer :: i,j,N
	real*8, allocatable :: xd(:), fd(:)
	real*8 :: t,z
	character*50 :: inputfile
	
	! JANGAN LUPA DEFINISIKAN FUNGSI EKSTERNAL
	! SESUAIKAN DENGAN FUNGSI YANG DIGUNAKAN DI CONTOH PENGGUNAAN
	real*8, external :: int_lagrange3
	!real*8, external :: int_lagrange3
	!real*8, external :: int_hermite3
	
	
	
! Proses pembacaan data masukan
	inputfile = "data.txt"
	
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
		write(*,*) xd(i)," ",fd(i)
	end do

	write(*,*)
	write(*,*)	"=======Estimasi Titik Data======="
	write(*,*) "Masukkan titik tinjau :"
	read(*,*) t

	
	!! CONTOH PENGGUNAAN
	!z = int_hermite3(t,N,xd,fd)
	!write(*,*) "Nilai pada titik tinjau :"
	!write(*,*) z
	
	!z = int_lagrange3(t,N,xd,fd)
	!write(*,*) "Nilai pada titik tinjau :"
	!write(*,*) z
	
	z = int_lagrange3(t,N,xd,fd)
	write(*,*) "Nilai pada titik tinjau :"
	write(*,*) z
	

	
	
	deallocate(xd)
	deallocate(fd)
	stop
	end program 