module fizzbuzz_test
    implicit none
    private

    public :: testGenerator
contains
    function testGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t, describe, it

        type(TestSuite_t) :: test_suite

        test_suite = describe("fizzbuzz", &
                [it("represents the normal numbers as strings", normalNumbersAreString)])
    end function testGenerator

    pure function normalNumbersAreString() result(test_result)
        use Fizzbuzz_m, only: fizzbuzz
        use Vegetables_m, only: TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        test_result = assertEquals("1", fizzbuzz(1))
    end function normalNumbersAreString
end module fizzbuzz_test
