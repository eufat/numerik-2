	program hitung_spl_m
	implicit none
		
	integer :: N,i,j,k,M,r
	real*8, allocatable :: A(:,:),B(:,:),X(:,:),At(:),Ab(:,:),Bb(:,:),Bt(:)
	real*8 :: sum
	character*50 :: inputfile
	
	
	inputfile = "data_matrix_multicol.txt"
	
	! Baca file input
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N,M
	allocate(A(N,N))
	allocate(B(N,M))
	allocate(X(N,M))

	
	read(10,*)
	do i=1,N
		read(10,*) (A(i,j), j=1,N)
	end do
	read(10,*)
	do i=1,N
		read(10,*) (B(i,j), j=1,M)
	end do
	close(10)
	
	! Tulis data dari file input ke layar
	write(*,*)
	write(*,*) "Matriks yang diinput : "
	write(*,*) "Ukuran :",N,"variabel, ", M,"sistem persamaan linear"
	write(*,*)
	write(*,*) "Matriks A : "
	do i=1,N
		write(*,*) (A(i,j), j=1,N)
	end do
	write(*,*)
	write(*,*) "Matriks B : "
	do i=1,N
		write(*,*) (B(i,j), j=1,M)
	end do
	write(*,*)

	call ludcmp_m(N,M,A,B,X)
	!call elgauss_m(N,M,A,B,X)
	
	write(*,*)
	write(*,*) "Solusi sistem persamaan:"
	write(*,*)
	write(*,*) "Solusi sistem persamaan:"
	do r=1,M
		do i=1,N
			write(*,*) "X(",i,",",r,")= ",X(i,r)
		end do
		write (*,*)
	end do
	
	write(*,*) "Matriks X : "
	do i=1,N
		write(*,*) (X(i,j), j=1,M)
	end do
	
	
	deallocate(A)
	deallocate(B)
	deallocate(X)	
	stop
	end program
	
	