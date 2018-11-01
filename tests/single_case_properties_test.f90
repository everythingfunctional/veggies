module single_case_properties_test
    implicit none
    private

    public :: test_case_properties
contains
    function test_case_properties() result(test)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test

        test = describe("A test case", &
                [it("includes the given description", checkCaseDescription), &
                it("Only has 1 test case", checkNumCases)])
    end function test_case_properties

    function checkCaseDescription() result(result_)
        use example_cases_m, only: examplePassingTestCase, EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = examplePassingTestCase()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_case%description())
    end function checkCaseDescription

    function checkNumCases() result(result_)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: Result_t, TestCase_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = examplePassingTestCase()
        result_ = assertEquals(1, test_case%numCases())
    end function checkNumCases
end module single_case_properties_test
