module test_case_test
    implicit none
    private

    character(len=*), parameter :: EXAMPLE_DESCRIPTION = "Example Description"

    public :: test_case_properties
contains
    pure function test_case_properties() result(test)
        use Vegetables_m, only: TestCollection_t, Describe, It

        type(TestCollection_t) :: test

        test = Describe("A test case", &
                [It("includes the given description", caseDescriptionCheck), &
                It("Only has 1 test case", checkNumCases)])
    end function test_case_properties

    pure function caseDescriptionCheck() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = exampleTestCase()
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_case%description())
    end function caseDescriptionCheck

    pure function checkNumCases() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertEquals

        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        test_case = exampleTestCase()
        result_ = assertEquals(1, test_case%numCases())
    end function checkNumCases

    pure function exampleTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, It, succeed

        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION, succeed)
    end function exampleTestCase
end module test_case_test
