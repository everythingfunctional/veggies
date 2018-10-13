module Fizzbuzz_m
    implicit none
    private

    public :: fizzbuzz
contains
    pure function fizzbuzz(number) result(result_)
        integer, intent(in) :: number
        character(len=:), allocatable :: result_

        character(len=32) :: temp

        if (mod(number, 15) == 0) then
            result_ = "fizzbuzz"
        else if (mod(number, 3) == 0) then
            result_ = "fizz"
        else if (mod(number, 5) == 0) then
            result_ = "buzz"
        else
            write(temp, '(I0)') number
            result_ = trim(temp)
        end if
    end function fizzbuzz
end module Fizzbuzz_m
