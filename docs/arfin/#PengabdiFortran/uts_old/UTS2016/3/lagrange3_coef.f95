	program interpolasi_lagrange3
	implicit none
	integer :: i,j,k,N,M,c
	real*8, allocatable :: xd(:), fd(:),xmin, xmax, dx, prod, p, x
	
	character*50 :: inputfile
	
! Proses pembacaan data masukan
	write(*,*) "Masukkan nama file data  :"
	read(*,*) inputfile
	write(*,*)
	
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

	open(unit=12, file="hasil_intLagrange3_"//inputfile, status="replace", action="write")
	write(*,*)
	write(*,*)	"=======Koefisien Interpolasi Lagrange Kubik======="
	
write(12,*) "# "
write(12,*)	"# =======Koefisien Interpolasi Lagrange Kubik======="

	do i=1,N-2
	
		write(12,*)	"## Interval",xd(i),"-",xd(i+1),"-",xd(i+2)
		write(12,*) "a0	= ", a0
		write(12,*) "a1	= ", a0
		write(12,*) "a2	= ", a0
		write(12,*) "a3	= ", a0
		write(12,*)
	end do
	
	write(*,*)
	write(*,*) "~~Proses Selesai~~"
	write(*,*) "Silakan cek titik data pada : hasil_intLagrange3_"//inputfile
	
	close(12)
	
	deallocate(xd)
	deallocate(fd)
	stop
	end program interpolasi_lagrange3