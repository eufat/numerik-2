PROGRAM interpolasilagrange3

implicit none
integer, parameter :: dpr=kind(1.0D5)
integer :: N, M, i
real(dpr), allocatable :: xd(:), fd(:)
character*50 :: inputfile, outputfile

write (*,*) 'INTERPOLASI LAGRANG ORDE 3'

inputfile = "datano2.txt"
outputfile = "hasil_intepolasi_kubik.txt"
 
open(unit=10, file=inputfile, status="old", action="read")
read(10,*) N
allocate(xd(N))
allocate(fd(N))
 
read(10,*)
do i=1,N
 read(10,*) xd(i), fd(i)
end do
close(10)
 do i=1,N
 write(*,*) xd(i)," ",fd(i)
end do
write (*,*) 'Masukkan jumlah titik :'
read (*,*) M

call lagrange3(N,xd,fd,M)
deallocate(xd)
deallocate(fd)
write(*,*) "Data hasil interpolasi ada pada :", outputfile

stop

contains

subroutine lagrange3(n,x,f,m)
implicit none
real(dpr), dimension(n) :: x, f
integer :: i, j, k, n, m, c
real(dpr) :: dx, l, x0, p

open(unit=12, file=outputfile, status="replace", action="write")

dx=(x(n)-x(1))/(m-1)
do k=0,m
 x0=x(1)+k*dx
 p=0.
 do c=1,n-1
  if(x0<=x(c)) exit
 end do
 do i=c-2,c+1
  l=1.
  do j=c-2,c+1
   if(j==i) cycle
   l=l*(x0-x(j))/(x(i)-x(j))
  end do
  p=p+l*f(i)
  write(*,*) f(i)
 end do
 write (12,*) x0," ", p
end do  
 
close(12)
  
end subroutine lagrange3

END PROGRAM interpolasilagrange3