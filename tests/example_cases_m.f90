module example_cases_m
    implicit none
    private

    character(len=*), parameter, public :: EXAMPLE_DESCRIPTION = "Example Description"

    public :: exampleFailingTestCase, examplePassingTestCase, runCase
contains
    function examplePassingTestCase() result(test_case)
        use example_asserts_m, only: exampleMultipleAsserts
        use Vegetables_m, only: TestCase_t, TestCase

        type(TestCase_t) :: test_case

        test_case = TestCase(EXAMPLE_DESCRIPTION, exampleMultipleAsserts)
    end function examplePassingTestCase

    function exampleFailingTestCase() result(test_case)
        use example_asserts_m, only: exampleMultipleAssertsWithFail
        use Vegetables_m, only: TestCase_t, TestCase

        type(TestCase_t) :: test_case

        test_case = TestCase(EXAMPLE_DESCRIPTION, exampleMultipleAssertsWithFail)
    end function exampleFailingTestCase

    function runCase(example_case) result(example_result)
        use Vegetables_m, only: TestCase_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_case
        type(Transformed_t) :: example_result

        select type (example_case)
        type is (TestCase_t)
            example_result = Transformed(example_case%run())
        class default
            example_result = Transformed(fail("Expected to get a TestCase_t"))
        end select
    end function runCase
end module example_cases_m
