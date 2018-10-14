module vegetables_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t, describe, it

        type(TestSuite_t) :: test_suite

        test_suite = describe("TestSuite_t", &
                [it("knows how many test cases it has", suiteForNumCases)])
    end function testGenerator

    pure function suiteForNumCases() result(test_result)
        use Vegetables_m, only: TestResult_t, TestSuite_t, assertEquals

        type(TestResult_t) :: test_result

        type(TestSuite_t) :: test_suite

        test_suite = exampleGenerator()
        test_result = assertEquals(1, test_suite%numCases())
    end function suiteForNumCases

    pure function exampleGenerator() result(test_suite)
        use Vegetables_m, only: TestSuite_t, describe, it

        type(TestSuite_t) :: test_suite

        test_suite = describe("examples", &
                [it("Can compare integers", compareIntegers)])
    end function exampleGenerator

    pure function compareIntegers() result(test_result)
        use Vegetables_m, only: TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        test_result = assertEquals(1, 1)
    end function compareIntegers
end module vegetables_test
