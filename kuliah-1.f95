program sorting
    implicit none
    integer, parameter :: N=10
    integer :: i,j
    character(8) :: nama(N), nama_urut(N)
    real*8 :: nilai(N), nilai_urut(N), biggest

    open(unit=20, file="nilai.dat", status="old")
    do i=1,N
        read(20,*) nama(i), nilai(i)
    end do
    close(20)

    write(*,*)
    write(*,*) "Sebelum diurutkan: "
    write(*,*)

    do i=1,N
        write(*,*) nama(i), nilai(i)
    end do

    do j=1,1
        biggest=0.0
        do i=1,N
            if(nilai(i) > biggest) then
                biggest=nilai(i)
                nama_urut(j)=nama(i)
            end if
        end do
    end do

    write(*,*)
    write(*,*) "Urutan nama dengan nilai tertinggi: "
    write(*,*)

    do j=1,1
        write(*,*) nama_urut(j), biggest
    end do

end program