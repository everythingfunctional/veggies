module single_case_properties_test
    implicit none
    private

    public :: test_case_properties
contains
    function test_case_properties() result(test)
        use example_cases_m, only: examplePassingTestCase
        use Helpers_m, only: TestItemInput_t
        use Vegetables_m, only: TestItem_t, describe, it_

        type(TestItem_t) :: test

        type(TestItem_t) :: individual_tests(3)
        type(TestItemInput_t) :: the_case

        the_case%input = examplePassingTestCase()
        individual_tests(1) = it_("includes the given description", checkCaseDescription)
        individual_tests(2) = it_("only has 1 test case", checkNumCases)
        individual_tests(3) = it_("takes less than 3 times as long as the assertions to run", checkSpeed)
        test = describe("A test case", the_case, individual_tests)
    end function test_case_properties

    function checkCaseDescription(example_case) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Helpers_m, only: TestItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_case
        type(Result_t) :: result_

        select type (example_case)
        class is (TestItemInput_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_case%input%description())
        class default
            result_ = fail("Expected to get a TestItemInput_t")
        end select
    end function checkCaseDescription

    function checkNumCases(example_case) result(result_)
        use Helpers_m, only: TestItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_case
        type(Result_t) :: result_

        select type (example_case)
        class is (TestItemInput_t)
            result_ = assertEquals(1, example_case%input%numCases())
        class default
            result_ = fail("Expected to get a TestItemInput_t")
        end select
    end function checkNumCases

    function checkSpeed(example_case) result(result_)
        use Helpers_m, only: TestItemInput_t
        use Vegetables_m, only: Input_t, Result_t, TestItem_t, assertFasterThan, fail

        class(Input_t), intent(in) :: example_case
        type(Result_t) :: result_

        type(TestItem_t) :: internal_case

        select type (example_case)
        type is (TestItemInput_t)
            internal_case = example_case%input
            result_ = assertFasterThan(runAssertions, runCase, 100)
        class default
            result_ = fail("Expected to get a TestItemInput_t")
        end select
    contains
        subroutine runCase
            use Vegetables_m, only: TestResultItem_t

            integer :: i
            type(TestResultItem_t) :: internal_result

            do i = 1, 100
                internal_result = internal_case%run()
            end do
        end subroutine runCase

        subroutine runAssertions
            use example_asserts_m, only: exampleMultipleAsserts
            use Vegetables_m, only: Result_t

            integer :: i
            type(Result_t) :: result__

            do i = 1, 300
                result__ = exampleMultipleAsserts()
            end do
        end subroutine runAssertions
    end function checkSpeed
end module single_case_properties_test
