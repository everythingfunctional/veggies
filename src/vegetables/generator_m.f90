module vegetables_generator_m
    implicit none
    private
    public :: generator_t

    type, abstract :: generator_t
    contains
        private
        procedure(generate_i), public, deferred :: generate
        procedure(shrink_i), nopass, public, deferred :: shrink
    end type

    abstract interface
        function generate_i(self) result(generated_value)
            use vegetables_generated_m, only: generated_t
            import :: generator_t

            implicit none

            class(generator_t), intent(in) :: self
            type(generated_t) :: generated_value
        end function

        function shrink_i(input) result(shrunk)
            use vegetables_input_m, only: input_t
            use vegetables_shrink_result_m, only: shrink_result_t

            implicit none

            class(input_t), intent(in) :: input
            type(shrink_result_t) :: shrunk
        end function
    end interface
end module
