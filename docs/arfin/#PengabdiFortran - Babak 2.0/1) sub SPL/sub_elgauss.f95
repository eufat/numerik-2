subroutine elgauss(N_,A_,B_,X_)
	implicit none	
	integer, intent(in) :: N_
	real*8 :: A_(N_,N_),B_(N_),X_(N_)
	integer :: N,i,j,k
	real*8, allocatable :: A(:,:),B(:),X(:),At(:),Ab(:,:),Bb(:)
	real*8 :: Bt,sum
		
	! Dummy variable to input
	N = N_
	allocate(A(N,N))
	allocate(B(N))
	allocate(X(N))
	allocate(At(N))
	allocate(Ab(N,N))
	allocate(Bb(N))
	
	A=A_
	B=B_
	X=X_
	

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
	
	X_ = X
	
	deallocate(A)
	deallocate(B)
	deallocate(X)
	deallocate(At)
	deallocate(Ab)
	deallocate(Bb)
	return
	end subroutine
	
	