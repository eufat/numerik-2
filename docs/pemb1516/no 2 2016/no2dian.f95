		program LUdcmp
		implicit none
		integer :: i,j,N,M,p,k,r
		real*8, allocatable :: A(:,:),B(:),X(:)
		real*8, allocatable :: L(:,:),U(:,:),Y(:,:),Ls(:),As(:)
		real*8, allocatable :: dummy(:)
		real*8 :: dummy0 , Bs
		
		character*50 :: nama_file_data
		
		nama_file_data="datano2.txt"
		
		! Buka file data
		open(unit=10, file=nama_file_data, status="old")
		
		! Baca data matrix
		read(10,*) N
		
		allocate(A(N,N))
		allocate(B(N))
		allocate(X(N))
		allocate(Ls(N))
		allocate(As(N))
		allocate(dummy(N))
				
		read(10,*)
		
		do i=1,N
			read(10,*) (A(i,j), j=1,N)
		end do
		read(10,*) 
		do i=1,N
			read(10,*) B(i)
		end do
		
		close(10)
		
		! Tampilkan data
		write(*,*) N
		write(*,*)
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		write(*,*)
		do i=1,N
			write(*,*) (B(i))
		end do
		
		
		! Triangulasi matriks A menjadi matriks triangular
		do k=1,N-1
			do i=k+1,N
			dummy0=A(i,k)/A(k,k)
				do j=k,N
				A(i,j)=A(i,j)-dummy0*A(k,j)
				end do
			end do
	
		
			! Jika ada elemen diagonal L yang berharga nol, maka tukar baris L tsb
			! dengan baris dibawahnya.
			if (A(k+1,k+1)==0.0) then
			
				As(:)=A(k+1,:)
				A(k+1,:)=A(k+2,:)
				A(k+2,:)=As(:)
			
				Bs=B(k+1)
				B(k+1)=B(k+2)
				B(k+2)=Bs								

			end if	
		end do
		
		! Tampilkan matrix A
		
		write(*,*)
		write(*,*) "Matrix A (triangular) ="
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		write(*,*)	
		write(*,*) "Matrix B akibat triangulasi ="
		do i=1,N
			write(*,*) B(i)
		end do

		
		! Mencari matrix X dengan substitusi mundur
		X(N)=B(N)/A(N,N)
		do j=1,N-1
			dummy0=0.d0
				do k=N-j+1,N
					dummy0=dummy0+A(N-j,k)*X(k)
				end do
				X(N-j)=(B(N-j)-dummy0)/A(N-j,N-j)
		end do
		
		! Tampilkan matrix X
		write(*,*)
		write(*,*) "Matrix X:"
		do i=1,N
			write(*,*) X(i)
		end do
		write(*,*)		
		
				
		deallocate(A)
		deallocate(B)
		deallocate(X)
		deallocate(Ls)
		deallocate(As)
		deallocate(dummy)
		
		! Tampilkan jawaban 2i
		write (*,*)
		write (*,*) "arus masing-masing hambatan"
		write (*,*) "arus (200 ohm)= " , X(1)
		write (*,*) "arus (80 ohm)= " , X(1)-X(2)
		write (*,*) "arus (80 ohm)= " , X(2)-X(3)
		write (*,*) "arus (80 ohm)= " , X(3)
		
		end program