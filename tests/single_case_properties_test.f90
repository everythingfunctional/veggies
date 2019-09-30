module single_case_properties_test
    implicit none
    private

    public :: test_case_properties
contains
    function test_case_properties() result(test)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: TestItem_t, describe, it_

        type(TestItem_t) :: test

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it_("includes the given description", checkCaseDescription)
        individual_tests(2) = it_("only has 1 test case", checkNumCases)
        individual_tests(3) = it_("takes very little time to run", checkSpeed)
        test = describe("A test case", examplePassingTestCase(), individual_tests)
    end function test_case_properties

    function checkCaseDescription(example_case) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCase_t, assertIncludes, fail

        class(*), intent(in) :: example_case
        type(Result_t) :: result_

        select type (example_case)
        class is (TestCase_t)
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
        class is (TestCase_t)
            result_ = assertEquals(1, example_case%numCases())
        class default
            result_ = fail("Expected to get a TestCase_t")
        end select
    end function checkNumCases

    function checkSpeed(example_case) result(result_)
        use Vegetables_m, only: Result_t, SimpleTestCase_t, assertFasterThan, fail

        class(*), intent(in) :: example_case
        type(Result_t) :: result_

        type(SimpleTestCase_t) :: internal_case

        select type (example_case)
        type is (SimpleTestCase_t)
            internal_case = example_case
            result_ = assertFasterThan(1.0d-6, runCase, 100)
        class default
            result_ = fail("Expected to get a TestCase_t")
        end select
    contains
        subroutine runCase
            use Vegetables_m, only: TestCaseResult_t

            class(TestCaseResult_t), allocatable :: internal_result

            internal_result = internal_case%run()
        end subroutine
    end function checkSpeed
end module single_case_properties_test
