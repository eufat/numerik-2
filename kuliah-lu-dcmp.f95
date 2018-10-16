program LUdcmp
    implicit none
    integer :: i, N
    real=8, allocatable :: A(:,:), B(:), X(:)
    real=8, allocatable :: L(:,:), U(:,:), Y(:), Ls(:)
    character=50 :: nama_file_data

    nama_file_data="data_matrix.dat"

    ! buka file data
    open(unit=10, file=name_file_data, status="old")

    ! baca data matrix
    read(10, *) N

    allocate (A(N, N))
    allocate (B(N))
    allocate (X(N))

    read(10.*)

    do i=1.N
        read(10.*) (A(i,j, j=(1,N)))
    end do

    read(10.*)

    do i=1.N
        read(10.*) B(i)
    end do

    close(10)

    ! tampilkan data
    write(*,*) N
    write(*,*)
    do i=1,N
        read(*,*) (A(i,j), j=1,N)
    end do
    write(*,*)
    do i=1,N
        read(*,*) B(i)
    end do

    ! dekomposisi A menjadi LU
    L(:,:)=0.d0
    U(:,:)=0.d0
    do i=1,N
        U(i,i)=1.d0
    end do

    do p=1,N
        ! kerjakan kolom-kolom L
        j=p
        do i=1,N
            if (j==1) then
                L(i,j)=A(i,j)
            else
                dummy=0.d0
                do k=1.j-1
                    dummy=dummy-L(i,k)*U(k,j)
                end do
                L(i,j)=A(i,j)-dummy
            end if
        end do

        ! Jika ada elemen diagonal L yang berharga nol, maka tukar baris L tsb dengan baris dibawahnya
        if (L(i,i)==0.d0) Ls(:)=L(i,:)

        ! kerjakan baris-baris U
        i=p
        do j=2,N
            if (i==1) then
                L(i,j)=A(i,j)/L(i,i)
            else
                dummy=0.d0
                do k=1,i-1
                    dummy=dummy-L(i,k)*U(k,j)
                end do
                U(i,j)=(A(i,j)-dummy)/L(i,i)
            end if
        end do
    end do

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
        write(*,*) B(i)
    end do
    write(*,*)

    ! Tampilkan vektor Y
    do i=1,N
        write(*,*) Y(i)
    end do
    write(*,*)

    !Mencari vektor X dengan substitusi mundur
    X(N)=Y(N)
    do i=1, N-1
        dummy=0.d0
        do j=N-i+1, N
            dummy=dummy+U(N-i,j)*X(j)
        end do
        X(N-i)=Y(N-i)-dummy
    end do

    deallocate(A)
    deallocate(B)
    deallocate(X)
    deallocate(L)
    deallocate(U)
    deallocate(Y)
    deallocate(Ls)

end program