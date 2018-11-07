program LUdcmp
implicit none
integer :: i,j,N,M,p,k,r,c
real*8, allocatable :: A(:,:),B(:,:),X(:,:)
real*8, allocatable :: L(:,:),U(:,:),Y(:,:),Ls(:),As(:),Xs(:)
real*8, allocatable :: Bs(:), dummy0, dummy(:)

character*50 :: nama_file_data

nama_file_data="data_matrix_12.dat"

! Buka file data
open(unit=10, file=nama_file_data, status="old")

! Baca data matrix
read(10,*) N, M

allocate(A(N,N))
allocate(B(N,M))
allocate(X(N,M))
allocate(L(N,N))
allocate(U(N,N))
allocate(Y(N,M))
allocate(Ls(N))
allocate(As(N))
allocate(Bs(M))
allocate(Xs(M))
allocate(dummy(M))

read(10,*)

do i=1,N
    read(10,*) (A(i,j), j=1,N)
end do
read(10,*)
do i=1,N
    read(10,*) (B(i,j), j=1,M)
end do

close(10)

! Tampilkan data
write(*,*) N, M
write(*,*)
do i=1,N
    write(*,*) (A(i,j), j=1,N)
end do
write(*,*)
do i=1,N
    write(*,*) (B(i,j), j=1,M)
end do

! Dekomposisi A menjadi LU

L(:,:)=0.d0
U(:,:)=0.d0
do i=1,N
    U(i,i)=1.d0
end do

do p=1,N

! Kerjakan kolom-kolom L
j=p


do i=j,N
    if (j==1) then
        L(i,1)=A(i,1)
    else
        dummy0=0.0
        do k=1,j-1
            dummy0=dummy0+L(i,k)*U(k,j)
        end do
        L(i,j)=A(i,j)-dummy0
    end if
end do


! Jika ada elemen diagonal L yang berharga nol, maka tukar baris L tsb
! dengan baris dibawahnya.
if (L(p,p)==0.0) then
    c=p

    Ls(:)=L(p,:)
    L(p,:)=L(p+1,:)
    L(p+1,:)=Ls(:)

    As(:)=A(p,:)
    A(p,:)=A(p+1,:)
    A(p+1,:)=As(:)

    Bs(:)=B(p,:)
    B(p,:)=B(p+1,:)
    B(p+1,:)=Bs(:)
end if


! Kerjakan baris-baris matriks U
i=p
do j=i,N
    if (i==1) then
        U(1,j)=A(1,j)/L(1,1)
    else
        dummy0=0.0
        do k=1,i-1
            dummy0=dummy0+L(i,k)*U(k,j)
        end do
        U(i,j)=(A(i,j)-dummy0)/L(i,i)
    end if
end do

end do ! p

! Tampilkan matrix L, U, dan B'
write(*,*)
write(*,*) "Matrix L ="
do i=1,N
    write(*,*) (L(i,j), j=1,N)
end do
write(*,*)
write(*,*) "Matrix U ="
do i=1,N
    write(*,*) (U(i,j), j=1,N)
end do
write(*,*)
write(*,*) "Matrix B (sesudah penukaran baris) ="
do i=1,N
    write(*,*) (B(i,j), j=1,M)
end do
write(*,*)
write(*,*) "Matrix A (sesudah penukaran baris) ="
do i=1,N
    write(*,*) (A(i,j), j=1,N)
end do
write(*,*)

! Mencari matrix Y dengan substitusi maju
Y(1,:)=B(1,:)/L(1,1)
do i=2,N
    do r=1,M
        dummy(r)=0.d0
        do j=1,i-1
            dummy(r)=dummy(r)+L(i,j)*Y(j,r)
        end do
        Y(i,r)=(B(i,r)-dummy(r))/L(i,i)
    end do
end do

! Tampilkan matrix Y
write(*,*) "Matrix Y:"
do i=1,N
    write(*,*) (Y(i,j), j=1,M)
end do
write(*,*)

! Mencari matrix X dengan substitusi mundur
X(N,:)=Y(N,:)
do i=1,N-1
    do r=1,M
        dummy(r)=0.d0
        do j=N-i+1,N
            dummy(r)=dummy(r)+U(N-i,j)*X(j,r)
        end do
        X(N-i,r)=Y(N-i,r)-dummy(r)
    end do
end do

! Tukar baris X sesuai konsekuensi perubahan A
do i=1,N-1
    if (c == N) then
        Xs(:)=X(N,:)
        X(N,:)=X(N+1,:)
        X(N+1,:)=Xs(:)
    end if
end do

! Tampilkan matrix X
write(*,*) "Matrix X:"
do i=1,N
    write(*,*) (X(i,j), j=1,M)
end do
write(*,*)


deallocate(A)
deallocate(B)
deallocate(X)
deallocate(L)
deallocate(U)
deallocate(Y)
deallocate(Ls)
deallocate(As)
deallocate(Bs)
deallocate(Xs)
deallocate(dummy)

end program