module fizzbuzz_test
    implicit none
    private

    public :: testGenerator, normalNumbersAreString
contains
    pure function testGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t

        type(TestSuite_t) :: test_suite

        associate(a => test_suite); end associate
    end function testGenerator

    pure function normalNumbersAreString() result(test_result)
        use Vegetables_m, only: TestResult_t

        type(TestResult_t) :: test_result

        associate(a => test_result); end associate
    end function normalNumbersAreString
end module fizzbuzz_test
