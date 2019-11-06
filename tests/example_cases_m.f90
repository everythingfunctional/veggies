module example_cases_m
    use example_asserts_m, only: &
            exampleMultipleAsserts, exampleMultipleAssertsWithFail
    use Vegetables_m, only: TestItem_t, It

    implicit none
    private

    character(len=*), parameter, public :: EXAMPLE_DESCRIPTION = "Example Description"
    character(len=*), parameter, public :: NOT_IN_DESCRIPTION = "NOT"

    public :: exampleFailingTestCase, examplePassingTestCase
contains
    function examplePassingTestCase() result(test_case)
        type(TestItem_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION, exampleMultipleAsserts)
    end function examplePassingTestCase

    function exampleFailingTestCase() result(test_case)
        type(TestItem_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION, exampleMultipleAssertsWithFail)
    end function exampleFailingTestCase
end module example_cases_m
