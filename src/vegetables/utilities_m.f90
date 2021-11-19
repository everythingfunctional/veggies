module vegetables_utilities_m
    implicit none
    private
    public :: equals_within_absolute
contains
    elemental function equals_within_absolute(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equals_within_absolute

        equals_within_absolute = abs(expected - actual) <= tolerance
    end function
end module
