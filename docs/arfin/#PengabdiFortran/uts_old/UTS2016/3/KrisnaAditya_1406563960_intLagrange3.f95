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
	write(*,*)	"=======Estimasi Titik Data======="
	write(*,*) "Masukkan batas awal :"
	read(*,*) xmin
	write(*,*) "Masukkan batas akhir :"
	read(*,*) xmax
	write(*,*) "Masukkan jumlah titik :"
	read(*,*) M
	
write(12,*) "# "
write(12,*)	"# =======Estimasi Titik Data======="
write(12,*) "# Batas awal :", xmin
write(12,*) "# Batas akhir :", xmax
write(12,*) "# Jumlah titik :", M
write(12,*) "# x			f(x)"

	dx=(xmax-xmin)/ real(M, 8)
	
	do k=0,M
		x = xmin + real(k, 8)*dx	
		p = 0.d0
		do c=1,N
			if (x<=xd(c)) exit
		end do
		do i=c-2,c+1
			prod = 1.d0
			do j=c-2,c+1
				if(j==i) cycle
				prod = prod *(x-xd(j)) / (xd(i)-xd(j)) 
			end do
			p = p+prod*fd(i)
		end do
		write(12,*) x,"			", p
	end do
	
	write(*,*)
	write(*,*) "~~Proses Selesai~~"
	write(*,*) "Silakan cek titik data pada : hasil_intLagrange3_"//inputfile
	
	close(12)
	
	deallocate(xd)
	deallocate(fd)
	stop
	end program interpolasi_lagrange3