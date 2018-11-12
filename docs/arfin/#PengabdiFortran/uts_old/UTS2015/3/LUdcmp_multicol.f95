	program lu_decomposition
	implicit none
	
	real*8, allocatable :: A(:,:),L(:,:),U(:,:),B(:,:)
	real*8, allocatable :: X(:,:), Y(:,:), Ls(:), As(:),Bs(:)
	real*8 :: sum
	integer :: i,j,k,p,r,N,M
	character*50 :: inputfile
	
	write(*,*) "Masukkan nama file data matriks :"
	read(*,*) inputfile

	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N,M
	allocate(A(N,N))
	allocate(L(N,N))
	allocate(U(N,N))
	allocate(B(N,M))
	allocate(X(N,M))
	allocate(Y(N,M))
	allocate(Ls(N))
	allocate(As(N))
	allocate(Bs(M))
	
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
	
	U=0.0
	do i=1,N
		U(i,i) = 1.0
	end do
	
	L=0.0
	
	do p=1,N
		j=p
		
			do i=j,N
				if(j==1) then
					L(i,1) = A(i,1)
				else
					sum = 0.0
					do k=1,j-1
						sum = sum+L(i,k)*U(k,j)
					end do
					L(i,j) = A(i,j) - sum
				end if
			end do
		
		if (L(p,p) == 0.0) then
			Ls(:) = L(p,:)
			L(p,:) = L(p+1,:)
			L(p+1,:) = Ls(:)
			
			As(:) = A(p,:)
			A(p,:) = A(p+1,:)
			A(p+1,:) = As(:)
			
			Bs(:) = B(p,:)
			B(p,:) = B(p+1,:)
			B(p+1,:) = Bs(:)
			
		end if
		
		i=p
		do j=i,N
			if(i==1) then
				U(1,j)=A(1,j)/L(1,1)
			else
				sum = 0.0
				do k=1,i-1
					sum = sum+L(i,k)*U(k,j)
				end do
				U(i,j) = (A(i,j)-sum)/L(i,i)
			end if
		end do
	
	end do
	
	do r=1,M
		Y(1,r) = B(1,r)/L(1,1)
	end do
	
	do r=1,M
		do i=2,N
			sum=0.0
			do j=1,i-1
				sum = sum+L(i,j)*Y(j,r)
			end do
			Y(i,r) = (B(i,r)-sum)/L(i,i)
		end do
	end do
	
	do r=1,M
		X(N,r) = Y(N,r)
	end do
	
	do r=1,M
		do i=1,N-1
			sum =0.0
			do j=N-i+1,N
				sum = sum+U(N-i,j)*X(j,r)
			end do
			X(N-i,r) =Y(N-i,r)-sum
		end do
	end do
	!	Tulis ke layar
	write(*,*) "===Hasil Dekomposisi LU==="
	write(*,*) 	
	write(*,*) "Matriks L: "
	do i=1,N
		write(*,*) (L(i,j), j=1,N)
	end do
	
	write(*,*) 
	write(*,*) "Matriks U: "
	do i=1,N
		write(*,*) (U(i,j), j=1,N)
	end do
	
	
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
	
	! Tulis ke file
	open(unit=11, file="Solusi_LU_"//inputfile, status="replace", action="write")
	write(11,*) "Matriks yang diinput : "
	write(11,*) "Ukuran :",N,"variabel, ", M,"sistem persamaan linear"
	write(11,*)
	write(11,*) "Matriks A : "
	do i=1,N
		write(11,*) (A(i,j), j=1,N)
	end do
	write(11,*)
	write(11,*) "Matriks B : "
	do i=1,N
		write(11,*) (B(i,j), j=1,M)
	end do
	write(11,*)
	write(11,*) "===Hasil Dekomposisi LU==="
	write(11,*) 	
	write(11,*) "Matriks L: "
	do i=1,N
		write(11,*) (L(i,j), j=1,N)
	end do
	
	write(11,*) 
	write(11,*) "Matriks U: "
	do i=1,N
		write(11,*) (U(i,j), j=1,N)
	end do
	
	
	write(11,*)
	write(11,*) "Solusi sistem persamaan:"
	do r=1,M
		do i=1,N
			write(11,*) "X(",i,",",r,")= ",X(i,r)
		end do
		write(11,*)
	end do

	write(11,*) "Matriks X : "
	do i=1,N
		write(11,*) (X(i,j), j=1,M)
	end do
	
	close(11)
	
	deallocate(A)
	deallocate(L)
	deallocate(U)
	deallocate(B)
	deallocate(X)
	deallocate(Y)
	deallocate(Ls)
	deallocate(As)
	deallocate(Bs)

	stop
	end program lu_decomposition