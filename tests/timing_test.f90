module timing_test
    implicit none
    private

    public :: test_timing
contains
    function test_timing() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("is fast", checkSpeedOfDivide)
        tests = describe("division", individual_tests)
    end function test_timing

    function checkSpeedOfDivide() result(result_)
        use Vegetables_m, only: Result_t, assertFasterThan

        type(Result_t) :: result_

        result_ = assertFasterThan(1d-6, doDivision, 1000)
    contains
        subroutine doDivision
            double precision :: numerator
            double precision :: denomenator
            double precision :: quotient

            numerator = 4.0d0
            denomenator = 2.0d0
            quotient = numerator / denomenator
        end subroutine doDivision
    end function checkSpeedOfDivide
end module timing_test
