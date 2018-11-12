	program hitung_spl
	implicit none
		
	integer :: N,i,j,k
	real*8, allocatable :: A(:,:),B(:),X(:)
	character*50 :: inputfile
	
	
	inputfile = "data_matrix.txt"
	
	! Baca file input
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N
	allocate(A(N,N))
	allocate(B(N))
	allocate(X(N))
	
	read(10,*)
	do i=1,N
		read(10,*) (A(i,j), j=1,N)
	end do
	read(10,*)
	do i=1,N
		read(10,*) B(i)
	end do
	close(10)
	
	! Tulis data dari file input ke layar
	write(*,*)
	write(*,*) "Matriks yang diinput : "
	write(*,*) "Ukuran :",N,"variabel"
	write(*,*)
	write(*,*) "Matriks A : "
	do i=1,N
		write(*,*) (A(i,j), j=1,N)
	end do
	write(*,*)
	write(*,*) "Matriks B : "
	do i=1,N
		write(*,*) B(i)
	end do
	
	!======Call Subroutine 
	!	Pilih salah satu
	call elgauss(N,A,B,X)
	!call ludcmp(N,A,B,X)
	
	
	
	! Tampilkan isi X
	write(*,*)
	write(*,*) "Solusi sistem persamaan:"
	do i=1,N
		write(*,*) "X(",i,")= ",X(i)
	end do

	
	deallocate(A)
	deallocate(B)
	deallocate(X)
	stop
	end program
	
	