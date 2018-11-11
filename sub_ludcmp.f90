subroutine sub_ludcmp(N,A,B,X)
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