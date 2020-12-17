module vegetables_test_collection_with_input_m
    use iso_varying_string, only: varying_string
    use vegetables_input_m, only: input_t
    use vegetables_test_m, only: test_t
    use vegetables_test_item_m, only: test_item_t

    implicit none
    private
    public :: test_collection_with_input_t, test_collection_with_input

    type, extends(test_t) :: test_collection_with_input_t
        private
        type(varying_string) :: description_
        type(test_item_t), allocatable :: tests(:)
        class(input_t), allocatable :: input
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure :: run_with_input
        procedure :: run_without_input
    end type
contains
    function test_collection_with_input(description, input, tests)
        use iso_varying_string, only: varying_string
        use vegetables_input_m, only: input_t
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_collection_with_input_t) :: test_collection_with_input

        test_collection_with_input%description_ = description
        allocate(test_collection_with_input%input, source = input)
        allocate(test_collection_with_input%tests, source = tests)
    end function

    pure recursive function description(self)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, join, NEWLINE
        use vegetables_constants_m, only: INDENTATION

        class(test_collection_with_input_t), intent(in) :: self
        type(varying_string) :: description

        integer :: i

        description = hanging_indent( &
                self%description_ // NEWLINE // join( &
                        [(self%tests(i)%description(), i = 1, size(self%tests))], &
                        NEWLINE), &
                INDENTATION)
    end function

    pure recursive function filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)
        use vegetables_test_m, only: filter_result_t
        use vegetables_test_item_m, only: filter_item_result_t, test_item_t

        class(test_collection_with_input_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        type(test_collection_with_input_t) :: new_collection
        type(filter_item_result_t) :: filter_results(size(self%tests))
        integer :: i
        logical :: matches(size(self%tests))
        type(test_item_t) :: maybe_tests(size(self%tests))

        if (self%description_.includes.filter_string) then
            filter_result%matched = .true.
            allocate(filter_result%test, source = self)
        else
            filter_results = [(self%tests(i)%filter(filter_string), i = 1, size(self%tests))]
            if (any(filter_results%matched)) then
                matches = filter_results%matched
                maybe_tests = filter_results%test
                new_collection = self
                deallocate(new_collection%tests)
                allocate(new_collection%tests, source = &
                        pack(maybe_tests, mask=matches))
                filter_result%matched = .true.
                allocate(filter_result%test, source = new_collection)
            else
                filter_result%matched = .false.
            end if
        end if
    end function

    pure recursive function num_cases(self)
        class(test_collection_with_input_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%tests(i)%num_cases(), i = 1, size(self%tests))])
    end function

    recursive function run_with_input(self, input) result(result_)
        use vegetables_input_m, only: input_t
        use vegetables_test_result_item_m, only: test_result_item_t

        class(test_collection_with_input_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(unused => input)
        end associate

        result_ = self%run()
    end function

    recursive function run_without_input(self) result(result_)
        use vegetables_test_collection_result_m, only: test_collection_result
        use vegetables_test_result_item_m, only: test_result_item_t

        class(test_collection_with_input_t), intent(in) :: self
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))

        do i = 1, size(self%tests)
            results(i) = self%tests(i)%run(self%input)
        end do
        allocate(result_%result_, source = test_collection_result( &
                self%description_, results))
    end function
end module
