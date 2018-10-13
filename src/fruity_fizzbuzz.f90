program fruity_fizzbuzz
    use Fizzbuzz_m, only: fizzbuzz

    implicit none

    integer :: i

    do i = 1, 100
        print '(A)', fizzbuzz(i)
    end do
end program fruity_fizzbuzz
