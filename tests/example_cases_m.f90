module example_cases_m
    implicit none
    private

    character(len=*), parameter, public :: EXAMPLE_DESCRIPTION = "Example Description"

    public :: examplePassingTestCase
contains
    function exampleAssert() result(result_)
        use Vegetables_m, only: Result_t, succeed, toString

        type(Result_t) :: result_

        result_ = succeed(toString("Success"))
    end function exampleAssert
    function examplePassingTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, TestCase, succeed

        type(TestCase_t) :: test_case

        test_case = TestCase(EXAMPLE_DESCRIPTION, exampleAssert)
    end function examplePassingTestCase
end module example_cases_m
