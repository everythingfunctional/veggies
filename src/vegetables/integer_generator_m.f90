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

        type(integer_input_t) :: the_input

        associate(unused => self)
        end associate

        the_input%value_ = get_random_integer()
        generated_value = generated_t(the_input)
    end function

    pure function shrink(input) result(shrunk)
        use vegetables_input_m, only: input_t
        use vegetables_integer_input_m, only: integer_input_t
        use vegetables_shrink_result_m, only: &
                shrink_result_t, shrunk_value, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(integer_input_t) :: new_input

        select type (input)
        type is (integer_input_t)
            if (input%value_ == 0) then
                new_input%value_ = 0
                shrunk = simplest_value(new_input)
            else
                new_input%value_ = input%value_ / 2
                shrunk = shrunk_value(new_input)
            end if
        end select
    end function
end module
