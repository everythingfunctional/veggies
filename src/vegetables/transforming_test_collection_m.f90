module vegetables_transforming_test_collection_m
    use iso_varying_string, only: varying_string
    use vegetables_test_m, only: test_t
    use vegetables_test_interfaces_m, only: transformer_i
    use vegetables_test_item_m, only: test_item_t

    implicit none
    private
    public :: transforming_test_collection_t

    type, extends(test_t) :: transforming_test_collection_t
        private
        type(varying_string) :: description_
        type(test_item_t), allocatable :: tests(:)
        procedure(transformer_i), nopass, pointer :: transformer
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type

    interface transforming_test_collection_t
        module procedure constructor
    end interface
contains
    function constructor(description, transformer, tests) result(transforming_test_collection)
        use iso_varying_string, only: varying_string
        use vegetables_test_interfaces_m, only: transformer_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(transforming_test_collection_t) :: transforming_test_collection

        transforming_test_collection%description_ = description
        transforming_test_collection%transformer => transformer
        allocate(transforming_test_collection%tests, source = tests)
    end function

    pure recursive function description(self)
        use iso_varying_string, only: varying_string, operator(//)
        use strff, only: hanging_indent, join, NEWLINE
        use vegetables_constants_m, only: INDENTATION

        class(transforming_test_collection_t), intent(in) :: self
        type(varying_string) :: description

        integer :: i

        description = hanging_indent( &
                self%description_ // NEWLINE // join( &
                        [(self%tests(i)%description(), i = 1, size(self%tests))], &
                        NEWLINE), &
                INDENTATION)
    end function

    recursive function filter(self, filter_string) result(filter_result)
        use iso_varying_string, only: varying_string
        use strff, only: operator(.includes.)
        use vegetables_test_m, only: &
                filter_result_t, filter_failed, filter_matched
        use vegetables_test_item_m, only: filter_item_result_t, test_item_t

        class(transforming_test_collection_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        type(transforming_test_collection_t) :: new_collection
        type(filter_item_result_t) :: filter_results(size(self%tests))
        integer :: i
        logical :: matches(size(self%tests))
        type(test_item_t) :: maybe_tests(size(self%tests))

        if (self%description_.includes.filter_string) then
            filter_result = filter_matched(self)
        else
            filter_results = [(self%tests(i)%filter(filter_string), i = 1, size(self%tests))]
            if (any(filter_results%matched())) then
                matches = filter_results%matched()
                maybe_tests = filter_results%test()
                new_collection = self
                deallocate(new_collection%tests)
                allocate(new_collection%tests, source = &
                        pack(maybe_tests, mask=matches))
                filter_result = filter_matched(new_collection)
            else
                filter_result = filter_failed()
            end if
        end if
    end function

    pure recursive function num_cases(self)
        class(transforming_test_collection_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%tests(i)%num_cases(), i = 1, size(self%tests))])
    end function

    recursive function run_with_input(self, input) result(result_)
        use vegetables_input_m, only: input_t
        use vegetables_test_case_result_m, only: test_case_result
        use vegetables_test_collection_result_m, only: test_collection_result
        use vegetables_test_result_item_m, only: test_result_item_t
        use vegetables_transformation_failure_m, only: transformation_failure_t
        use vegetables_transformed_m, only: transformed_t

        class(transforming_test_collection_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))
        type(transformed_t) :: transformed_

        transformed_ = self%transformer(input)
        select type (transformed_input => transformed_%input)
        type is (transformation_failure_t)
            allocate(result_%result_, source = test_case_result( &
                    self%description_, transformed_input%result_))
        class default
            do i = 1, size(self%tests)
                results(i) = self%tests(i)%run(transformed_input)
            end do
            allocate(result_%result_, source = test_collection_result( &
                    self%description_, results))
        end select
    end function

    function run_without_input(self) result(result_)
        use vegetables_result_m, only: fail
        use vegetables_test_case_result_m, only: test_case_result
        use vegetables_test_result_item_m, only: test_result_item_t

        class(transforming_test_collection_t), intent(in) :: self
        type(test_result_item_t) :: result_

        allocate(result_%result_, source = test_case_result( &
                self%description_, fail("No input provided")))
    end function
end module
