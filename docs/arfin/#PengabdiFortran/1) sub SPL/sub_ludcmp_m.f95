subroutine ludcmp_m(N_,M_,A_,B_,X_)
	implicit none
	integer, intent(in) :: N_,M_
	real*8 :: A_(N_,N_),B_(N_,M_),X_(N_,M_)
	real*8, allocatable :: A(:,:),L(:,:),U(:,:),B(:,:)
	real*8, allocatable :: X(:,:), Y(:,:), Ls(:), As(:),Bs(:)
	real*8 :: sum
	integer :: i,j,k,p,r,N,M

	N = N_
	M = M_
	
	allocate(A(N,N))
	allocate(L(N,N))
	allocate(U(N,N))
	allocate(B(N,M))
	allocate(X(N,M))
	allocate(Y(N,M))
	allocate(Ls(N))
	allocate(As(N))
	allocate(Bs(M))
	
	A=A_
	B=B_
	X=X_
	 
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
	write(*,*) "Matriks Y:"
	do i=1,N
		write(*,*) (L(i,j), j=1,M)
	end do
	
	
	X_ = X
	
	deallocate(A)
	deallocate(L)
	deallocate(U)
	deallocate(B)
	deallocate(X)
	deallocate(Y)
	deallocate(Ls)
	deallocate(As)
	deallocate(Bs)

	return
	end subroutine