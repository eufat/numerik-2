	program hitung_interpolasi
	implicit none
	integer :: i,j,N,M
	real*8, allocatable :: xd(:), fd(:)
	real*8 :: xmin, xmax
	character*50 :: inputfile
	
! Proses pembacaan data masukan
	inputfile = "data_UTS17.txt"
	
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
	write(*,*) "Masukkan batas awal :"
	read(*,*) xmin
	write(*,*) "Masukkan batas akhir :"
	read(*,*) xmax
	write(*,*) "Masukkan jumlah titik :"
	read(*,*) M
	
	!call interpolasi_lagrange(N,xd,fd,xmin,xmax,M)
	!call interpolasi_lagrange3(N,xd,fd,xmin,xmax,M)
	call interpolasi_hermite3(N,xd,fd,xmin,xmax,M)

	deallocate(xd)
	deallocate(fd)
	stop
	end program 