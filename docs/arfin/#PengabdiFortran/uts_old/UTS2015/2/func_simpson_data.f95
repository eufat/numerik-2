	real*8 function simpson_data(fd_,idx_min,idx_max,h_,N_) result(z)
	implicit none
	real*8 :: fd_(N_),h_,h
	integer :: idx_min,idx_max
	integer :: i,N,N_,j
	real*8, allocatable :: w(:),fd(:),hasil
	
	N=idx_max-idx_min
	h=h_
	allocate(w(0:N))
	allocate(fd(0:N))
	
	j=0
	do i=idx_min,idx_max
		fd(j) = fd_(i)
		j=j+1
	end do
	
	!Tabulasi harga pemberat Simpson w(i)
	w(0)=h/3.d0
	w(N)=h/3.d0
	do i=1,N-1,2
		w(i)=4.d0*h/3.d0
	end do
	do i=2,N-2,2
		w(i)=2.d0*h/3.d0
	end do
	
	hasil=0.d0
	do i=0,N
		hasil=hasil+fd(i)*w(i)
	end do
	
	z=hasil
	
	deallocate(w)
	deallocate(fd)
	return
	
	end function