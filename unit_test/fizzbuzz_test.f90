module fizzbuzz_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t, describe, it

        type(TestSuite_t) :: test_suite

        test_suite = describe("fizzbuzz", &
                [it("represents the normal numbers as strings", normalNumbersAreString)])
    end function testGenerator

    pure function normalNumbersAreString() result(test_result)
        use Vegetables_m, only: TestResult_t

        type(TestResult_t) :: test_result

        associate(a => test_result); end associate
    end function normalNumbersAreString
end module fizzbuzz_test
