program sorting
    implicit none
    integer, parameter :: N=10
    integer :: i,j
    character(8) :: nama(N), nama_urut(N)
    real*8 :: nilai(N), nilai_urut(N)


! Buka file data bernama "nilai.dat" dan baca data:
    open(unit=20,file="nilai.dat",status="old")
    do i=1,N
        read(20,*) nama(i), nilai(i)
    end do
    close(20)

! Buka file hasil bernama "nilai_urut.dat":
    open(unit=30,file="nilai_urut.dat",status="unknown")

! Tulis hasil pembacaan ke layar:
    write(*,*)
    write(*,*) "Sebelum diurutkan:"
    write(*,*)
    do i=1,N
        write(*,*) nama(i), nilai(i)
    end do

! Tulis hasil pembacaan ke file bernama "nilai_urut.dat":
    write(30,*) "Sebelum diurutkan:"
    write(30,*)
    do i=1,N
        write(30,*) nama(i), nilai(i)
    end do

! Proses sorting (pengurutan nilai):
    nilai_urut(1)=-1.0
    do i=1,N
        if (nilai(i).gt.nilai_urut(1)) then
            nilai_urut(1)=nilai(i)
            nama_urut(1)=nama(i)
        end if
    end do


    do j=2,N
        nilai_urut(j)=-1.0
        do i=1,N
            if ((nilai(i).gt.nilai_urut(j)).and.(nilai(i).lt.nilai_urut(j-1))) then
                nilai_urut(j)=nilai(i)
                nama_urut(j)=nama(i)
            end if
        end do
    end do

! Tulis hasil sorting ke layar:
    write(*,*)
    write(*,*) "Urutan nama dengan nilai tertinggi:"
    write(*,*)
    do j=1,N
        write(*,*) nama_urut(j), nilai_urut(j)
    end do

! Tulis hasil sorting ke file bernama "nilai_urut.dat":
    write(30,*)
    write(30,*) "Urutan nama dengan nilai tertinggi:"
    write(30,*)
    do j=1,N
        write(30,*) nama_urut(j), nilai_urut(j)
    end do

end program