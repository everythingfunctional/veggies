module vegetables_string_input_m
    use iso_varying_string, only: varying_string
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: string_input_t

    type, extends(input_t) :: string_input_t
        type(varying_string) :: value_
    end type
end module
