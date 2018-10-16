module test_case_test
    implicit none
    private

    public :: test_can_be_single_case
contains
    pure function test_can_be_single_case() result(test)
        use Vegetables_m, only: Test_t, TODO

        class(Test_t), allocatable :: test

        test = TODO()
    end function test_can_be_single_case
end module test_case_test
