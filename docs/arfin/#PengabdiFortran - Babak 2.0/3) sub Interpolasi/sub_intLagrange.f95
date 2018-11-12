	subroutine interpolasi_lagrange(N_,xd_,fd_,xmin_, xmax_,M_)
	implicit none
	integer, intent(in) :: N_,M_
	real*8 :: xd_(N_),fd_(N_),xmin_,xmax_
	integer :: i,j,k,N,M
	real*8, allocatable :: xd(:), fd(:)
	real*8 :: xmin, xmax, dx, prod, p, x
	N = N_
	M = M_
	allocate(xd(N))
	allocate(fd(N))
	xd = xd_
	fd = fd_
	xmin = xmin_
	xmax = xmax_



open(unit=12, file="hasil_intLagrange.txt", status="replace", action="write")
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
		do i=1,N
			prod = 1.d0
			do j=1,N
				if(j==i) cycle
				prod = prod *(x-xd(j)) / (xd(i)-xd(j)) 
			end do
			p = p+prod*fd(i)
		end do
		write(12,*) x,"			", p
	end do
	
	write(*,*)
	write(*,*) "~~Proses Selesai~~"
	write(*,*) "Silakan cek titik data pada : hasil_intLagrange.txt"

	close(12)
	deallocate(xd)
	deallocate(fd)
	return
	end subroutine  