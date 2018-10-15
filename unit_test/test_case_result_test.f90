module test_case_result_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test_collection

        test_collection = describe("TestCaseResult_t", &
                [it("Knows if it passed", passedTestKnowsIt)])
    end function testGenerator

    pure function passedTestKnowsIt() result(test_result)
        use Vegetables_m, only: TestCaseResult_t, TestResult_t, assertThat

        type(TestResult_t) :: test_result

        type(TestCaseResult_t) :: passed_test

        passed_test = getPassedTest()
        test_result = assertThat(passed_test%passed())
    end function passedTestKnowsIt

    pure function getPassedTest() result(passed_test)
        use Vegetables_m, only: TestCase_t, TestCaseResult_t, it

        type(TestCaseResult_t) :: passed_test

        type(TestCase_t) :: test_case

        test_case = it("Passes", passing)
        passed_test = test_case%runCase()
    end function getPassedTest

    pure function passing() result(test_result)
        use Vegetables_m, only: TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        test_result = assertEquals(1, 1)
    end function passing
end module test_case_result_test
