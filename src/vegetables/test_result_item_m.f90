module vegetables_test_result_item_m
    use vegetables_test_result_m, only: test_result_t

    implicit none
    private
    public :: test_result_item_t

    type :: test_result_item_t
        class(test_result_t), allocatable :: result_
    contains
        private
        procedure, public :: num_asserts
        procedure, public :: num_cases
        procedure, public :: num_failing_asserts
        procedure, public :: num_failing_cases
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type
contains
    pure recursive function failure_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string

        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%failure_description(colorize)
    end function

    pure recursive function num_asserts(self)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_asserts()
    end function

    pure recursive function num_cases(self)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_cases()
    end function

    pure recursive function num_failing_asserts(self) result(num_asserts)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_failing_asserts()
    end function

    pure recursive function num_failing_cases(self) result(num_cases)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_failing_cases()
    end function

    pure recursive function passed(self)
        class(test_result_item_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function

    pure recursive function verbose_description( &
            self, colorize) result(description)
        use iso_varying_string, only: varying_string

        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%verbose_description(colorize)
    end function
end module
