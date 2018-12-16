		program Fitting
		implicit none
		
		integer, parameter :: N=10,M=5
		real*8 :: x(N),f(N),C(M+1,M+1),A(M+1),B(M+1)
		integer :: i,j,k,Np
		character*50 :: datafile,outputfile,datafile2
		real*8 :: dummy,xmin,xmax,xx,dx,p,sums
		
		
		datafile="data_2016.txt"
		datafile2="data_SI_2016.txt"
		outputfile="hasil_fitting.txt"
		Np=100
			
		
		! Baca data
		open(unit=10, file=datafile, status="old")
		do i=1,N
			read(10,*) x(i), f(i)
		end do
		
		
		
		do i=1,N
			write(*,*) x(i), f(i)
		end do
	
		open(unit=30, file=datafile2, status="unknown")
		do i=1,N
			write(30,*) x(i), f(i)/3.6
		end do
		f(:) = f(:)/3.6
		! Menghitung koefisien-koefisien C(k,j)
		do k=0,M
			do j=0,M
				dummy=0.d0
				do i=1,N
					dummy=dummy+x(i)**(j+k)
				end do
				C(k+1,j+1)=dummy
			end do
		end do
		
		! Menghitung koefisien-koefisien B(k)
		do k=0,M
			dummy=0.d0
			do i=1,N
				dummy=dummy+f(i)*x(i)**k
			end do
			B(k+1)=dummy
		end do
			
		! Menampilkan hasil penghitungan matriks C dan B
		write(*,*) 
		write(*,*) "Matriks C :"
		do i=0,M
			write(*,*) (C(i,j), j = 0,m)
		end do
		write(*,*)
		write(*,*) "Matriks B :"
		do i=0,M
			write(*,*) B(i)
		end do				

		call LUdcmp(M+1,C,B,A)

		write(*,*) 'nilai koef poliomial :',A

! 		Konstruksi polinomial p(x)

		open(unit=20, file=outputfile, status="unknown")
		
		xmin=X(1)
		xmax=X(N)
		dx=(xmax-xmin)/(Np-1)
		do i=1,Np
			xx=xmin+(i-1)*dx
			p=0.d0
			do j=1,M+1
				p=p+A(j)*xx**(j-1)
			end do
			write(20,*) xx, p
		end do
				
		
		write(*,*) "Perhitungan fitting selesai. Hasil disimpan dalam file ",outputfile
		
!		sums=0.!
!		do j=1,M+1
!			sums=sums+A(j)*47**(j-1)
!		end do
!		write(*,*) 'ini perkiraannya:', sums
!		stop
		
		
		contains
		
		subroutine LUdcmp(N,A,B,X)
		implicit none
		integer :: i,j,N,p,k
		real*8 :: A(N,N),B(N),X(N)
		real*8 :: L(N,N),U(N,N),Y(N),Ls(N),As(N)
		real*8 :: Bs, dummy
		
		
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
				dummy=0.0
				do k=1,j-1
					dummy=dummy+L(i,k)*U(k,j)
				end do
				L(i,j)=A(i,j)-dummy
			end if
		end do
				
		
		! Jika ada elemen diagonal L yang berharga nol, maka tukar baris L tsb
		! dengan baris dibawahnya.
		if (L(p,p)==0.0) then

			Ls(:)=L(p,:)
			L(p,:)=L(p+1,:)
			L(p+1,:)=Ls(:)
		
			As(:)=A(p,:)
			A(p,:)=A(p+1,:)
			A(p+1,:)=As(:)
		
			Bs=B(p)
			B(p)=B(p+1)
			B(p+1)=Bs								
		
		end if	
		
			
		! Kerjakan baris-baris matriks U
		i=p
		do j=i,N
			if (i==1) then
				U(1,j)=A(1,j)/L(1,1)
			else
				dummy=0.0
				do k=1,i-1
					dummy=dummy+L(i,k)*U(k,j)
				end do
				U(i,j)=(A(i,j)-dummy)/L(i,i)
			end if
		end do
		
		end do ! p
		

		! Mencari vektor Y dengan substitusi maju
		Y(1)=B(1)/L(1,1)
		do i=2,N
			dummy=0.d0
			do j=1,i-1
				dummy=dummy+L(i,j)*Y(j)
			end do
			Y(i)=(B(i)-dummy)/L(i,i)
		end do
		
		
		! Mencari vektor X dengan substitusi mundur
		X(N)=Y(N)
		do i=1,N-1
			dummy=0.d0
			do j=N-i+1,N
				dummy=dummy+U(N-i,j)*X(j)
			end do
			X(N-i)=Y(N-i)-dummy
		end do	
		
		return
		end subroutine
		end program
		