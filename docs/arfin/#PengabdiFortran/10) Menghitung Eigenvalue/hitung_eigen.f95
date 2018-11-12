	program hitung_eigen
	implicit none
	integer :: N,i,j,k
	real*8, allocatable :: M(:,:),E(:)
	real*8 :: emin, emax, step
	real*8, external :: det
	character*50 :: inputfile
	
	
	!inputfile = "data_matrix.txt"
	
	write(*,*) "Masukkan nama file matriks : "
	read(*,*) inputfile
	
	! Baca file input
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N
	allocate(M(N,N))
	allocate(E(N))
	
	E(:) = 0.d0
	
	read(10,*)
	do i=1,N
		read(10,*) (M(i,j), j=1,N)
	end do
	close(10)
	
	! Tulis data dari file input ke layar
	write(*,*)
	write(*,*) "Matriks yang diinput : "
	write(*,*) "Ukuran :",N,"x",N
	write(*,*)
	write(*,*) "Matriks M : "
	do i=1,N
		write(*,*) (M(i,j), j=1,N)
	end do
	write(*,*)
	
	write(*,*) "Masukkan batas minimum pencarian nilai eigen :"
	read(*,*) emin
	write(*,*) "Masukkan batas maksimum pencarian nilai eigen :"
	read(*,*) emax
	write(*,*) "Masukkan lebar langkah pencarian nilai eigen :"
	read(*,*) step
	
	
	!======Call Subroutine 
	
	call eigenvalue(M,N,emin,emax,step,E)
	write(*,*)
	do i=1,N
		write(*,*) "Nilai eigen ke-",i," :: ",E(i), "|| Bila dibulatkan :: ", NINT(E(i))
	end do

	
	deallocate(M)
	deallocate(E)
	stop
	end program
