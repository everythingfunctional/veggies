module fizzbuzz_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t

        type(TestSuite_t) :: test_suite

        associate(a => test_suite); end associate
    end function testGenerator
end module fizzbuzz_test
