subroutine eigenvalue(M_,N_,emin_,emax_,step_,E_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: M_(N_,N_),E_(N_), emin_,emax_, step_
	integer :: N,i,j,k
	real*8, allocatable :: M(:,:),E(:), Idt(:,:), C(:,:), window(:),res(:)
	real*8, parameter :: eps = 1.d-12
	real*8, external :: det, eval_range
	real*8 :: emin, emax, step

	! Dummy variable to input
	N = N_
	emin = emin_
	emax = emax_
	step = step_
	allocate(M(N,N))
	allocate(C(N,N))
	allocate(Idt(N,N))
	allocate(E(N))
	allocate(window(2))
	allocate(res(2))

	M=M_
	E=E_
	Idt(:,:) = 0.d0


	window(1) = emin
	window(2) = emin+step

	res(:) = 0.d0

	k=1

	do
		if (window(2)>emax) then
		exit
		end if
		if (k>N) then
		exit
		end if
	! set search range
		do j=1,2
			! subtitution
			do i=1,N
				Idt(i,i) = window(j)
			end do
			C = Idt-M
			! decompose using LU +  Calculate det L
			res(j) = det(C,N)
		end do

		if (res(1)*res(2)<eps)	then
			E(k) = eval_range(window, M, N)
			k=k+1
		end if

	! redefine range
		window(1) = window(2)
		window(2) = window(2)+step

	end do

	E_=E

	deallocate(M)
	deallocate(E)
	deallocate(Idt)
	deallocate(window)
	deallocate(res)

	return
end subroutine

real*8 function det(M_,N_) result(determinant)
	implicit none
	integer, intent(in) :: N_
	real*8 :: M_(N_,N_)
	real*8, allocatable :: A(:,:),L(:,:),U(:,:)
	real*8, allocatable :: Ls(:), As(:)
	real*8 :: sum,prod,detL
	integer :: i,j,k,p,r,N

	N=N_
	allocate(A(N,N))
	allocate(L(N,N))
	allocate(U(N,N))
	allocate(Ls(N))
	allocate(As(N))

	A = M_

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
	prod=1
	do i=1,N
		prod = prod*L(i,i)
	end do
	determinant = prod

	deallocate(A)
	deallocate(L)
	deallocate(U)
	deallocate(Ls)
	deallocate(As)

	return
	end function

real*8 function eval_range(range_,M_,N_) result (eigen)
	integer, intent(in) :: N_
	real*8 :: M_(N_,N_), range_(2)
	real*8 :: a,b, da,db,dc
	integer :: i,N
	real*8, allocatable :: M(:,:), Idt(:,:), C(:,:), window(:),res(:)
	real*8, parameter :: eps = 1.d-12
	real*8, external :: det

	a = range_(1)
	b = range_(2)
	N = N_
	allocate(M(N,N))
	allocate(Idt(N,N))
	allocate(C(N,N))

	M=M_
	Idt(:,:) = 0.d0
	C(:,:) = 0.d0

	do
		! determinan di A
		do i=1,N
			Idt(i,i) = a
		end do
		C = Idt-M
		da = det(C,N)
		! determinan di B
		do i=1,N
			Idt(i,i) = b
		end do
		C = Idt-M
		db = det(C,N)
		! determinan di antara A dan B
		do i=1,N
			Idt(i,i) = (a+b/2)
		end do
		C = Idt-M
		dc = det(C,N)

		if(da*dc<0.d0) then
			b = (a+b)/2
		else
			a = (a+b)/2
		end if

		if(abs(a-b)<= eps) then
			exit
		end if


	end do

	eigen = (a+b)/2

	deallocate(M)
	deallocate(Idt)

	deallocate(C)
	return
end function