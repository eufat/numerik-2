function string_gt(word_1,word_2) result(res)
    implicit none
    integer :: n
    character(len = 8), intent(in) :: word_1, word_2 ! input
    logical             :: res ! output

    do n = 1, len(word_1)
        if (word_1(n:n) > word_2(n:n)) then
            res = .true.
            exit
        else if (word_1(n:n) < word_2(n:n)) then
            res = .false.
            exit
        else if (word_1(n:n) == word_2(n:n)) then
            cycle
        end if
    end do
end function

program coba
    implicit none
    integer :: n
    character(len = 1) :: char_1, char_2, extracted_char
    character(len = 8) :: kata_1, kata_2
    logical :: string_gt

    ! char_1 = "z"
    ! char_2 = "a"

    ! if (char_1 > char_2) then
    !     print *, "char_1 > char_2"
    ! end if

    ! if (char_2 > char_1) then
    !     print *, "char_2 > char_1"
    ! end if

    kata_1 = "aaaaaaaa"
    kata_2 = "0aaaaaaa"

    print *, string_gt(kata_1, kata_2)
end program