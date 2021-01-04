module vegetables_integer_generator_m
    use vegetables_generator_m, only: generator_t

    implicit none
    private
    public :: INTEGER_GENERATOR

    type, extends(generator_t) :: integer_generator_t
    contains
        private
        procedure, public :: generate
        procedure, nopass, public :: shrink
    end type

    type(integer_generator_t), parameter :: &
            INTEGER_GENERATOR = integer_generator_t()
contains
    function generate(self) result(generated_value)
        use vegetables_generated_m, only: generated_t
        use vegetables_integer_input_m, only: integer_input_t
        use vegetables_random_m, only: get_random_integer

        class(integer_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(integer_input_t(get_random_integer()))
    end function

    function shrink(input) result(shrunk)
        use vegetables_input_m, only: input_t
        use vegetables_integer_input_m, only: integer_input_t
        use vegetables_shrink_result_m, only: &
                shrink_result_t, shrunk_value, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (integer_input_t)
            associate(input_val => input%input())
                if (input_val == 0) then
                    shrunk = simplest_value(integer_input_t(0))
                else
                    shrunk = shrunk_value(integer_input_t(input_val / 2))
                end if
            end associate
        end select
    end function
end module
