subroutine secant(f,a_input,b_input,itermax_input,toleransi_input,c)
	integer, optional,intent(in) :: itermax_input
	real*8 , optional, intent(in) :: toleransi_input
	real*8, intent(in) :: a_input,b_input
	real*8 :: c,toleransi,a,b, kesrel
	real*8, external :: f
	integer :: langkah, itermax

	! Dummy variable to input
	a=a_input
	b=b_input
	itermax = itermax_input
	toleransi = toleransi_input

	do langkah=1,itermax
		c = b -  (f(b)*(a-b))/(f(a)-f(b))
		kesrel = abs((c-b))

		if (kesrel <= toleransi) then
			write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah
			write(*,*) 'Akar    = ',c
			write(*,*) 'f(akar) = ',f(c)
			write(*,*) 'Kesalahan relatif =',kesrel
			write(*,*) 'Nilai kesalahan < toleransi'

			return
		end if
		a=b
		b=c

	end do
	write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah-1
	write(*,*) 'Akar    = ',c
	write(*,*) 'f(akar) = ',f(c)
	write(*,*) 'Kesalahan relatif =',kesrel
	return


end subroutine