! Mengetahui apakah string lebih kecil (secara index alfabetis) dari string lainnya
function string_lt(word_1,word_2) result(res)
    implicit none
    integer :: n
    character(len = 8), intent(in) :: word_1, word_2 ! input
    logical             :: res ! output

    do n = 1, len(word_1)
        if (word_1(n:n) < word_2(n:n)) then
            res = .true.
            exit
        else if (word_1(n:n) > word_2(n:n)) then
            res = .false.
            exit
        else if (word_1(n:n) == word_2(n:n)) then
            cycle
        end if
    end do
end function

program sorting
    implicit none
    integer, parameter :: N=10
    integer :: i,j
    character(8) :: nama(N), nama_urut(N)
    real*8 :: nilai(N), nilai_urut(N)
    logical :: string_lt


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

! Proses sorting (pengurutan nama):
    nama_urut(1)="zzzzzzzz" ! set to highest possible alphabet sentence
    do i=1,N
        if (string_lt(nama(i),nama_urut(1))) then
            nilai_urut(1)=nilai(i)
            nama_urut(1)=nama(i)
        end if
    end do


    do j=2,N
        nama_urut(j)="zzzzzzzz" ! set to highest possible alphabet sentence
        do i=1,N
            if ((string_lt(nama(i),nama_urut(j))).and..not.(string_lt(nama(i),nama_urut(j-1)))) then
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