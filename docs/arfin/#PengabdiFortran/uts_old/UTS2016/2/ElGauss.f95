	program eliminasi_gauss
	implicit none
		
	integer :: N,i,j,k
	real*8, allocatable :: A(:,:),B(:),X(:),At(:),Ab(:,:),Bb(:)
	real*8 :: Bt,sum
	character*50 :: inputfile
	
	
	inputfile = "data.txt"
	
	! Baca file input
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N
	allocate(A(N,N))
	allocate(B(N))
	allocate(X(N))
	allocate(At(N))
	allocate(Ab(N,N))
	allocate(Bb(N))
	
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
	

	! Proses triangulasi matrix A
	Ab=A
	Bb=B
	do k=1,N-1
		do i=k+1,N
			do j=k,N
				Ab(i,j)=A(i,j)-(A(i,k)/A(k,k))*A(k,j)
			end do
			Bb(i)=B(i)-(A(i,k)/A(k,k))*B(k)
		end do
		! Cek apakah A(k,k) berharga nol
		if (Ab(k+1,k+1)==0.d0) then
		! Tukar isi matrix A baris ke-k isi baris di bawahnya
			At(:)=Ab(k+1,:)
			Bt=Bb(k+1)
			Ab(k+1,:)=Ab(k+2,:)
			Ab(k+2,:)=At(:)
			Bb(k+1)=Bb(k+2)
			Bb(k+2)=Bt
		end if
		A=Ab
		B=Bb
	end do
	
	! Cek matriks A dan B hasil triangulasi
	write(*,*)	
	write(*,*) "===Hasil Eliminasi Gauss==="
	write(*,*) 
	write(*,*) "Matriks A:"
	do i=1,N
		write(*,*) (A(i,j), j=1,N)
	end do
	write(*,*)
	write(*,*) "Matriks B:"
		do i=1,N
		write(*,*) B(i)
	end do
	
	
	! Substitusi mundur	
	X(N)=B(N)/A(N,N)
	do j=1,N-1
		sum=0.d0
		do k=N-j+1,N
			sum=sum+A(N-j,k)*X(k)
		end do
		X(N-j)=(B(N-j)-sum)/A(N-j,N-j)
	end do
	
	! Tampilkan isi X
	write(*,*)
	write(*,*) "Solusi sistem persamaan:"
	do i=1,N
		write(*,*) "X(",i,")= ",X(i)
	end do
	

	! Tulis ke file
	
	open(unit=11, file="Solusi_ElGauss_"//inputfile, status="replace", action="write")
	
	write(11,*)
	write(11,*) "Matriks yang diinput : "
	write(11,*) "Ukuran :",N,"variabel"
	write(11,*)
	write(11,*) "Matriks A : "
	do i=1,N
		write(11,*) (A(i,j), j=1,N)
	end do
	write(11,*)
	write(11,*) "Matriks B : "
	do i=1,N
		write(11,*) B(i)
	end do
	write(11,*)
	
	write(11,*) "===Hasil Eliminasi Gauss==="
	write(11,*) 	
	write(11,*) "Matriks A : "
	do i=1,N
		write(11,*) (A(i,j), j=1,N)
	end do
	write(11,*)
	write(11,*) "Matriks B : "
	do i=1,N
		write(11,*) B(i)
	end do
	
	write(11,*)
	write(11,*) "Solusi sistem persamaan:"
	do i=1,N
		write(11,*) "X(",i,")= ",X(i)
	end do
	
	close(11)
	
	
read(*,*)
	
	deallocate(A)
	deallocate(B)
	deallocate(X)
	deallocate(At)
	deallocate(Ab)
	deallocate(Bb)
	stop
	end program
	
	