module Fizzbuzz_m
    implicit none
    private

    public :: fizzbuzz
contains
    pure function fizzbuzz(number) result(result_)
        integer, intent(in) :: number
        character(len=:), allocatable :: result_

        character(len=32) :: temp

        result_ = ""
        if (mod(number, 3) == 0) then
            result_ = result_ // "fizz"
        end if
        if (mod(number, 5) == 0) then
            result_ = result_ // "buzz"
        end if
        if (result_ == "") then
            write(temp, '(I0)') number
            result_ = trim(temp)
        end if
    end function fizzbuzz
end module Fizzbuzz_m
