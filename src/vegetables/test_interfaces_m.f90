module vegetables_test_interfaces_m
    implicit none
    private
    public :: computation_i, input_test_i, simple_test_i, transformer_i

    abstract interface
        subroutine computation_i
        end subroutine

        function input_test_i(input) result(result_)
            use vegetables_input_m, only: input_t
            use vegetables_result_m, only: result_t

            implicit none

            class(input_t), intent(in) :: input
            type(result_t) :: result_
        end function

        function simple_test_i() result(result_)
            use vegetables_result_m, only: result_t

            implicit none

            type(result_t) :: result_
        end function

        function transformer_i(input) result(output)
            use vegetables_input_m, only: input_t
            use vegetables_transformed_m, only: transformed_t

            implicit none

            class(input_t), intent(in) :: input
            type(transformed_t) :: output
        end function
    end interface
end module
