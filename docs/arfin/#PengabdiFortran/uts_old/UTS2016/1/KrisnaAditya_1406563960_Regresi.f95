	program regresi
	implicit none
	integer :: i,j,k,N,m,p
	real*8, allocatable :: xd(:), fd(:),C(:,:), B_(:), B(:),A(:,:),X_(:),At(:),Ab(:,:),Bb(:),Bt, x, y, dx, xmin, xmax, sum
	
	character*50 :: inputfile
	character*2  ::orde

! Proses pembacaan data masukan
	write(*,*) "Masukkan nama file data  :"
	read(*,*) inputfile
	write(*,*)
	
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N	
	allocate(xd(N))
	allocate(fd(N))
		
	read(10,*)
	do i=1,N
		read(10,*) xd(i), fd(i)
	end do
	close(10)

! Menampilkan hasil pembacaan data masukan	
	write(*,*) "Hasil baca file input"
	do i=1,N
		write(*,*) xd(i)," ",fd(i)
	end do
	
! Menentukan orde polinomial 
	write(*,*)
	write(*,*) "Orde polinomial yang diinginkan : "
	read(*,*)	orde
	read(orde,*) m

! Menyiapkan logfile
open(unit=12, file="log_orde"//orde//"_"//inputfile, status="replace", action="write")	
write(12,*) "Orde polinomial : ", orde


! Mengalokasikan matriks
	allocate(C(0:m,0:m))
	allocate(B_(0:m))
	allocate(A(m+1,m+1))
	allocate(X_(m+1))
	allocate(At(m+1))
	allocate(Ab(m+1,m+1))
	allocate(Bb(m+1))
	allocate(B(m+1))

	
	
! Proses penghitungan matriks C A = B
	do k=0,m
		do j=0,m
			sum = 0
			do i=1,N
				sum = sum + (xd(i)**(j+k))
			end do
			C(k,j)=sum
		end do
		
		sum = 0
		do i=1,N
			sum = sum + (fd(i)*(xd(i)**k))
		end do
		B_(k) = sum
	end do

! Menampilkan hasil penghitungan matriks C dan B
	write(*,*) 
	write(*,*) "Matriks C :"
	do i=0,m
		write(*,*) (C(i,j), j = 0,m)
	end do
	write(*,*)
	write(*,*) "Matriks B :"
	do i=0,m
		write(*,*) B_(i)
	end do	
	
	
write(12,*) 
write(12,*) "Matriks C :"
do i=0,m
	write(12,*) (C(i,j), j = 0,m)
end do
write(12,*)
write(12,*) "Matriks B :"
do i=0,m
	write(12,*) B_(i)
end do	
	
	
!======Eliminasi Gauss=============

	! Perlu penyesuaian indeks matriks hasil 
	! LeastSquare agar dapat masuk ke program
	! EliminasiGauss
	
	! Copy C ke A
	do i=0,m
		do j=0,m
		A(i+1,j+1) = C(i,j)
		end do
	end do
	! Copy B_ ke B
	do i=0,m
		B(i+1) = B_(i)
	end do
		
	N=m+1
	Ab=A
	Bb=B

! Proses triangulasi matrix A
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
	write(*,*)	"=======Hasil Eliminasi Gauss======="
	write(*,*) "Matriks C:"
	do i=1,N
		write(*,*) (A(i,j), j=1,N)
	end do
	write(*,*)
	write(*,*) "Matriks B:"
		do i=1,N
		write(*,*) B(i)
	end do
	

write(12,*)
write(12,*)	"=======Hasil Eliminasi Gauss======="
write(12,*) "Matriks C:"
do i=1,N
	write(12,*) (A(i,j), j=1,N)
end do
write(12,*)
write(12,*) "Matriks B:"
	do i=1,N
	write(12,*) B(i)
end do
		
	! Substitusi mundur	
	X_(N)=B(N)/A(N,N)
	do j=1,N-1
		sum=0.d0
		do k=N-j+1,N
			sum=sum+A(N-j,k)*X_(k)
		end do
		X_(N-j)=(B(N-j)-sum)/A(N-j,N-j)
	end do
	
	! Tampilkan isi X
	write(*,*)
	write(*,*) "Koefisien Polinomial:"
	do i=1,N
		write(*,*) "a(",i-1,")= ",X_(i)
	end do
	
write(12,*)
write(12,*) "Koefisien Polinomial:"
do i=1,N
	write(12,*) "a(",i-1,")= ",X_(i)
end do
	
	open(unit=11, file="hasil_orde"//orde//"_"//inputfile, status="replace", action="write")
	write(*,*)
	write(*,*)	"=======Estimasi Titik Data======="
	write(*,*) "Masukkan batas awal :"
	read(*,*) xmin
	write(*,*) "Masukkan batas akhir :"
	read(*,*) xmax
	write(*,*) "Masukkan jumlah titik :"
	read(*,*) p
	
write(12,*)
write(12,*)	"=======Estimasi Titik Data======="
write(12,*) "Batas awal :", xmin
write(12,*) "Batas akhir :", xmax
write(12,*) "Jumlah titik :", p
write(12,*)
	
	p=p-1
	dx=(xmax-xmin)/ real(p, 8)

	do k=0,p
		x = xmin + real(k, 8)*dx
		y = 0
		do i=0,m
			y = y + (X_(i+1))*(x**i)
		end do
		write(11,*) x,"			", y
		write(12,*) x,"			", y
	end do
	
	close(11)
	close(12)
	
	write(*,*)
	write(*,*) "~~Proses Selesai~~"
	write(*,*) "Silakan cek titik data pada : hasil_orde"//orde//"_"//inputfile
	write(*,*) "Logfile : log_orde"//orde//"_"//inputfile
	
	
	deallocate(C)
	deallocate(B)
	deallocate(A)
	deallocate(X_)
	deallocate(At)
	deallocate(Ab)
	deallocate(Bb)
	deallocate(xd)
	deallocate(fd)
	stop
	
	end program regresi