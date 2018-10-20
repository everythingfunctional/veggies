module test_case_test
    implicit none
    private

    public :: test_can_be_single_case, test_case_properties
contains
    pure function test_can_be_single_case() result(test)
        use Vegetables_m, only: Test_t, SUCCEEDS

        class(Test_t), allocatable :: test

        test = SUCCEEDS()
    end function test_can_be_single_case

    pure function test_case_properties() result(test)
        use Vegetables_m, only: Test_t, Describe, It

        class(Test_t), allocatable :: test

        test = Describe("A test case", &
                [It("includes the given description", caseDescriptionCheck)])
    end function test_case_properties

    pure function caseDescriptionCheck() result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes, It, succeed

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_DESCRIPTION = "Example Description"
        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION, succeed)
        result_ = assertIncludes(EXAMPLE_DESCRIPTION, test_case%description())
    end function caseDescriptionCheck
end module test_case_test
