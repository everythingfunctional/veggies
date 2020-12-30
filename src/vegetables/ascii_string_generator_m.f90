module vegetables_ascii_string_generator_m
    use vegetables_generator_m, only: generator_t

    implicit none
    private
    public :: ASCII_STRING_GENERATOR

    type, extends(generator_t) :: ascii_string_generator_t
    contains
        private
        procedure, public :: generate
        procedure, nopass, public :: shrink
    end type

    type(ascii_string_generator_t), parameter :: &
            ASCII_STRING_GENERATOR = ascii_string_generator_t()
contains
    function generate(self) result(generated_value)
        use vegetables_generated_m, only: generated_t
        use vegetables_random_m, only: get_random_ascii_string
        use vegetables_string_input_m, only: string_input_t

        class(ascii_string_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        type(string_input_t) :: the_input

        associate(unused => self)
        end associate

        the_input%value_ = get_random_ascii_string()
        generated_value = generated_t(the_input)
    end function

    pure function shrink(input) result(shrunk)
        use iso_varying_string, only: assignment(=), extract, len
        use vegetables_input_m, only: input_t
        use vegetables_shrink_result_m, only: &
                shrink_result_t, simplest_value, shrunk_value
        use vegetables_string_input_m, only: string_input_t

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(string_input_t) :: new_input

        select type (input)
        type is (string_input_t)
            if (len(input%value_) <= 1) then
                new_input%value_ = ""
                shrunk = simplest_value(new_input)
            else
                new_input%value_ = extract( &
                        input%value_, 1, len(input%value_) - 1)
                shrunk = shrunk_value(new_input)
            end if
        end select
    end function
end module
