

! Nama      : Rizky Achmad Kurnia
! NPM       : 1406556980

	program regresi_leastsquare
	implicit none

	real*8, allocatable :: tetha(:),E(:),A(:,:),B(:),C(:,:),B1(:),X1(:),Ap(:),Abaru(:,:),Bbaru(:),Bt,sum
	integer :: i,j,k,m,N,p


!Membaca input
	open(unit=10, file="data_regresi.txt", status="old", action="read")
	read(10,*) N
	allocate(tetha(N))
	allocate(E(N))

	read(10,*)
	do i=1,N
		read(10,*) tetha(i), E(i)
	end do
	close(10)

!Menampilkan hasil bacaan
	do i=1,N
		write(*,*) tetha(i)," ",E(i)
	end do

!Menentukan Orde Polinomial
	write(*,*)
	write(*,*) "Masukkan Orde Polinomial"
	read(*,*) m


!Alokasi matriks
	allocate(C(0:m,0:m))
	allocate(B1(0:m))

!Matriks untuk eliminasi gauss ukuran = m+1,m+1
	allocate(A(m+1,m+1))
	allocate(X1(m+1))
	allocate(Ap(m+1))
	allocate(Abaru(m+1,m+1))
	allocate(Bbaru(m+1))
	allocate(B(m+1))

!Looping untuk perhitungan matrix Least Square (C)(A)=(B)
	do k=0,m
		!Matriks C
		do j=0,m
			sum = 0
			do i=1,N
				sum = sum + (tetha(i)**(j+k))
			end do
			C(k,j)=sum
		end do
		!Matriks B
		sum = 0
		do i=1,N
			sum = sum + (E(i)*(tetha(i)**k))
		end do
		B1(k) = sum
	end do

!Menampilkan hasil  matriks C dan B
	write(*,*)
	write(*,*) "Matriks C :"
	do i=0,m
		write(*,*) (C(i,j), j = 0,m)
	end do
	write(*,*)
	write(*,*) "Matriks B :"
	do i=0,m
		write(*,*) B1(i)
	end do


!Eliminasi Gauss

!Matriks Gauss yaitu (A)(X)=(B)
!Perlu diubah indeks dari matriks least square [(C)(A)=(B)] menjadi matriks gauss [(A)(X)=(B)]

!Mengubah indeks matriks C ke A
	do i=0,m
		do j=0,m
		A(i+1,j+1) = C(i,j)
		end do
	end do
	! Copy B1 ke B
	do i=0,m
		B(i+1) = B1(i)
	end do

	N=m+1
	Abaru=A
	Bbaru=B

!Triangulasi matriks A
	do k=1,N-1
		do i=k+1,N
			do j=k,N
				Abaru(i,j)=A(i,j)-(A(i,k)/A(k,k))*A(k,j)
			end do
			Bbaru(i)=B(i)-(A(i,k)/A(k,k))*B(k)
		end do
!Mengecek matriks A bernilai nol
		if (Abaru(k+1,k+1)==0.d0) then
!Proses Pivoting
			Ap(:)=Abaru(k+1,:)
			Bt=Bbaru(k+1)
			Abaru(k+1,:)=Abaru(k+2,:)
			Abaru(k+2,:)=Ap(:)
			Bbaru(k+1)=Bbaru(k+2)
			Bbaru(k+2)=Bt
		end if
		A=Abaru
		B=Bbaru
	end do

!Menampilkan hasil matriks triangulasi A dan B
	write(*,*)
	write(*,*) "Matriks C:"
	do i=1,N
		write(*,*) (A(i,j), j=1,N)
	end do
	write(*,*)
	write(*,*) "Matriks B:"
		do i=1,N
		write(*,*) B(i)
	end do

!Proses Backsubstitution
	X1(N)=B(N)/A(N,N)
	do j=1,N-1
		sum=0.d0
		do k=N-j+1,N
			sum=sum+A(N-j,k)*X1(k)
		end do
		X1(N-j)=(B(N-j)-sum)/A(N-j,N-j)
	end do

!Menampilkan matriks koefisien
	write(*,*)
	write(*,*) "Koefisien"
	do i=1,N
		write(*,*) "a(",i-1,")= ",X1(i)
	end do

	deallocate(C)
	deallocate(B)
	deallocate(A)
	deallocate(X1)
	deallocate(Ap)
	deallocate(Abaru)
	deallocate(Bbaru)
	deallocate(tetha)
	deallocate(E)
	stop

	end program regresi_leastsquare
