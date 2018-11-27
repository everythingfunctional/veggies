module single_case_properties_test
    implicit none
    private

    public :: test_case_properties
contains
    function test_case_properties() result(test)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: TestItem_t, describe, it_

        type(TestItem_t) :: test

        test = describe("A test case", examplePassingTestCase(), &
                [it_("includes the given description", checkCaseDescription), &
                it_("only has 1 test case", checkNumCases)])
    end function test_case_properties

    function checkCaseDescription(example_case) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes, fail

        class(*), intent(in) :: example_case
        type(Result_t) :: result_

        select type (example_case)
        type is (TestCase_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_case%description())
        class default
            result_ = fail("Expected to get a TestCase_t")
        end select
    end function checkCaseDescription

    function checkNumCases(example_case) result(result_)
        use Vegetables_m, only: Result_t, TestCase_t, assertEquals, fail

        class(*), intent(in) :: example_case
        type(Result_t) :: result_

        select type (example_case)
        type is (TestCase_t)
            result_ = assertEquals(1, example_case%numCases())
        class default
            result_ = fail("Expected to get a TestCase_t")
        end select
    end function checkNumCases
end module single_case_properties_test
