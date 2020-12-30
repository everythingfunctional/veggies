module double_precision_generator_m
    use vegetables, only: generator_t

    implicit none
    private
    public :: DOUBLE_PRECISION_GENERATOR

    type, extends(generator_t) :: double_precision_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(double_precision_generator_t) :: DOUBLE_PRECISION_GENERATOR = &
            double_precision_generator_t()
contains
    function generate(self) result(random_double)
        use vegetables, only: &
                double_precision_input_t, &
                generated_t, &
                get_random_double_precision_with_magnitude

        class(double_precision_generator_t), intent(in) :: self
        type(generated_t) :: random_double

        type(double_precision_input_t) :: the_input

        associate(a => self)
        end associate

        the_input%value_ = get_random_double_precision_with_magnitude(1.0d12)
        random_double = generated_t(the_input)
    end function

    pure function shrink(input) result(shrunk)
        use vegetables, only: &
                double_precision_input_t, &
                input_t, &
                shrink_result_t, &
                shrunk_value, &
                simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(double_precision_input_t) :: new_input

        select type (input)
        type is (double_precision_input_t)
            if (effectively_zero(input%value_)) then
                new_input%value_ = 0.0d0
                shrunk = simplest_value(new_input)
            else
                new_input%value_ = input%value_ / 2.0d0
                shrunk = shrunk_value(new_input)
            end if
        end select
    end function

    pure function effectively_zero(value_)
        double precision, intent(in) :: value_
        logical :: effectively_zero

        effectively_zero = abs(value_) < epsilon(value_)
    end function
end module
