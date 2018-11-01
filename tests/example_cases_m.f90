module example_cases_m
    implicit none
    private

    character(len=*), parameter, public :: EXAMPLE_DESCRIPTION = "Example Description"
    character(len=*), parameter, public :: FAILURE_MESSAGE = "Failure Message"
    integer, parameter, public :: NUM_ASSERTS_IN_PASSING = 2

    public :: exampleFailingTestCase, examplePassingTestCase
contains
    function exampleMultipleAsserts() result(result_)
        use Vegetables_m, only: Result_t, operator(.and.), succeed

        type(Result_t) :: result_

        result_ = succeed().and.succeed()
    end function exampleMultipleAsserts

    function exampleMultipleAssertsWithFail() result(result_)
        use Vegetables_m, only: Result_t, operator(.and.), fail, succeed

        type(Result_t) :: result_

        result_ = succeed().and.fail(FAILURE_MESSAGE)
    end function exampleMultipleAssertsWithFail

    function examplePassingTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, it, succeed

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, exampleMultipleAsserts)
    end function examplePassingTestCase

    function exampleFailingTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, it, succeed

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, exampleMultipleAssertsWithFail)
    end function exampleFailingTestCase
end module example_cases_m
