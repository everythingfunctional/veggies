module vegetables_example_m
    use vegetables_input_m, only: input_t

    implicit none
    private
    public :: example_t, example

    type :: example_t
        class(input_t), allocatable :: input
    end type
contains
    pure function example(input)
        class(input_t), intent(in) :: input
        type(example_t) :: example

        allocate(example%input, source = input)
    end function
end module
