module Helpers_m
    use Vegetables_m, only: Input_t, TestItem_t, TestResultItem_t

    implicit none
    private

    type, public, extends(Input_t) :: TestItemInput_t
        type(TestItem_t) :: input
    end type TestItemInput_t

    type, public, extends(Input_t) :: TestResultItemInput_t
        type(TestResultItem_t) :: input
    end type TestResultItemInput_t

    public :: runTest
contains
    function runTest(example_test) result(example_result)
        use Vegetables_m, only: &
                Input_t, &
                TransformationFailure_t, &
                Transformed_t, &
                fail, &
                Transformed

        class(Input_t), intent(in) :: example_test
        type(Transformed_t) :: example_result

        type(TransformationFailure_t) :: failure
        type(TestResultItemInput_t) :: the_result

        select type (example_test)
        type is (TestItemInput_t)
            the_result%input = example_test%input%run()
            example_result = Transformed(the_result)
        class default
            failure%result_ = fail("Expected to get a TestItemInput_t")
            example_result = Transformed(failure)
        end select
    end function runTest
end module Helpers_m
