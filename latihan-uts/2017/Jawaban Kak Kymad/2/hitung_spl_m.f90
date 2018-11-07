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
	contains
	subroutine elgauss_m(N_,M_,A_,B_,X_)
	implicit none
	integer, intent(in) :: N_,M_
	real*8 :: A_(N_,N_),B_(N_,M_),X_(N_,M_)
	integer :: N,i,j,k,M,r
	real*8, allocatable ::A(:,:),B(:,:),X(:,:),At(:),Ab(:,:),Bb(:,:),Bt(:)
	real*8 :: sum

	N = N_
	M = M_

	allocate(A(N,N))
	allocate(B(N,M))
	allocate(X(N,M))
	allocate(At(N))
	allocate(Ab(N,N))
	allocate(Bb(N,M))
	allocate(Bt(M))

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
			do r=1,M
				Bb(i,r)=B(i,r)-(A(i,k)/A(k,k))*B(k,r)
			end do
		end do
		! Cek apakah A(k,k) berharga nol
		if (Ab(k+1,k+1)==0.d0) then
		! Tukar isi matrix A baris ke-k isi baris di bawahnya
			At(:)=Ab(k+1,:)
			Bt(:)=Bb(k+1,:)
			Ab(k+1,:)=Ab(k+2,:)
			Ab(k+2,:)=At(:)
			Bb(k+1,:)=Bb(k+2,:)
			Bb(k+2,:)=Bt(:)
		end if
		A=Ab
		B=Bb
	end do

	! Cek matriks A dan B hasil triangulasi
	write(*,*)
	write(*,*) "===Hasil Eliminasi Gauss==="
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

	! Substitusi mundur
	do r=1,M
		X(N,r)=B(N,r)/A(N,N)
	end do

	do r=1,M
		do j=1,N-1
			sum=0.d0
			do k=N-j+1,N
				sum=sum+A(N-j,k)*X(k,r)
			end do
			X(N-j,r)=(B(N-j,r)-sum)/A(N-j,N-j)
		end do
	end do

	X_ = X

	deallocate(A)
	deallocate(B)
	deallocate(X)
	deallocate(At)
	deallocate(Ab)
	deallocate(Bb)
	deallocate(Bt)
	return
	end subroutine

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


	end program

