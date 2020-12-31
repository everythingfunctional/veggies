module helpers_m
    use vegetables, only: input_t, test_item_t, test_result_item_t

    implicit none
    private
    public :: test_item_input_t, test_result_item_input_t, run_test

    type, extends(input_t) :: test_item_input_t
        type(test_item_t) :: input
    end type

    type, extends(input_t) :: test_result_item_input_t
        type(test_result_item_t) :: input
    end type
contains
    function run_test(example_test) result(example_result)
        use vegetables, only: &
                input_t, &
                transformed_t, &
                transformation_failure_t, &
                fail

        class(input_t), intent(in) :: example_test
        type(transformed_t) :: example_result

        type(test_result_item_input_t) :: the_result

        select type (example_test)
        type is (test_item_input_t)
            the_result%input = example_test%input%run()
            example_result = transformed_t(the_result)
        class default
            example_result = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function
end module
