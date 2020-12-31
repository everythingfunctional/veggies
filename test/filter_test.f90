module filter_test
    use vegetables, only: input_t, filter_item_result_t

    implicit none
    private
    public :: test_filter_case, test_filter_collection

    type, extends(input_t) :: filter_item_result_input_t
        private
        type(filter_item_result_t) :: input_
    contains
        private
        procedure, public :: input
    end type

    interface filter_item_result_input_t
        module procedure constructor
    end interface
contains
    function test_filter_case() result(tests)
        use example_cases_m, only: example_passing_test_case
        use helpers_m, only: test_item_input_t
        use vegetables, only: test_item_t, given, then__, when

        type(test_item_t) :: tests

        type(test_item_t) :: collection(2)
        type(test_item_input_t) :: the_case
        type(test_item_t) :: first(1)
        type(test_item_t) :: second(1)

        the_case%input = example_passing_test_case()
        first(1) = then__("it doesn't match", check_case_not_matching)
        second(1) = then__("it returns itself", check_case_is_same)
        collection(1) = when( &
                "it is filtered with a string it doesn't contain", &
                filter_case_not_matching, &
                first)
        collection(2) = when( &
                "it is filtered with a matching string", &
                filter_case_matching, &
                second)
        tests = given("a test case", the_case, collection)
    end function

    function test_filter_collection() result(tests)
        use example_collections_m, only: example_passing_collection
        use helpers_m, only: test_item_input_t
        use vegetables, only: test_item_t, given, then__, when

        type(test_item_t) :: tests

        type(test_item_t) :: collection(3)
        type(test_item_input_t) :: the_collection
        type(test_item_t) :: first(1)
        type(test_item_t) :: second(1)
        type(test_item_t) :: third(1)

        the_collection%input = example_passing_collection()
        first(1) = then__("it doesn't match", check_collection_not_matching)
        second(1) = then__("it returns itself", check_collection_is_same)
        third(1) = then__("it returns a collection with only that case", check_collection_single_case)
        collection(1) = when( &
                "it is filtered with a string it doesn't contain", &
                filter_collection_not_matching, &
                first)
        collection(2) = when( &
                "it is filtered with a string matching its description", &
                filter_collection_matching_description, &
                second)
        collection(3) = when( &
                "it is filtered with a string matching only 1 of its cases", &
                filter_collection_matching_case, &
                third)
        tests = given("a test collection", the_collection, collection)
    end function

    function filter_case_not_matching(example_case) result(filtered)
        use example_cases_m, only: NOT_IN_DESCRIPTION
        use helpers_m, only: test_item_input_t
        use iso_varying_string, only: var_str
        use vegetables, only: &
                input_t, &
                transformation_failure_t, &
                transformed_t, &
                fail

        class(input_t), intent(in) :: example_case
        type(transformed_t) :: filtered

        select type (example_case)
        type is (test_item_input_t)
            filtered = transformed_t(filter_item_result_input_t( &
                    example_case%input%filter(var_str(NOT_IN_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_case_matching(example_case) result(filtered)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use helpers_m, only: test_item_input_t
        use iso_varying_string, only: var_str
        use vegetables, only: &
                input_t, &
                transformation_failure_t, &
                transformed_t, &
                fail

        class(input_t), intent(in) :: example_case
        type(transformed_t) :: filtered

        select type (example_case)
        type is (test_item_input_t)
            filtered = transformed_t(filter_item_result_input_t( &
                    example_case%input%filter(var_str(EXAMPLE_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_not_matching(example_collection) result(filtered)
        use example_collections_m, only: NOT_IN_DESCRIPTIONS
        use helpers_m, only: test_item_input_t
        use iso_varying_string, only: var_str
        use vegetables, only: &
                input_t, &
                transformation_failure_t, &
                transformed_t, &
                fail

        class(input_t), intent(in) :: example_collection
        type(transformed_t) :: filtered

        select type (example_collection)
        type is (test_item_input_t)
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%input%filter(var_str(NOT_IN_DESCRIPTIONS))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_matching_description(example_collection) result(filtered)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use helpers_m, only: test_item_input_t
        use iso_varying_string, only: var_str
        use vegetables, only: &
                input_t, &
                transformation_failure_t, &
                transformed_t, &
                fail

        class(input_t), intent(in) :: example_collection
        type(transformed_t) :: filtered

        select type (example_collection)
        type is (test_item_input_t)
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%input%filter(var_str(EXAMPLE_COLLECTION_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_matching_case(example_collection) result(filtered)
        use example_collections_m, only: EXAMPLE_CASE_DESCRIPTION_1
        use helpers_m, only: test_item_input_t
        use iso_varying_string, only: var_str
        use vegetables, only: &
                input_t, &
                transformation_failure_t, &
                transformed_t, &
                fail

        class(input_t), intent(in) :: example_collection
        type(transformed_t) :: filtered

        select type (example_collection)
        type is (test_item_input_t)
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%input%filter(var_str(EXAMPLE_CASE_DESCRIPTION_1))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function check_case_not_matching(input) result(result_)
        use vegetables, only: input_t, result_t, assert_not, fail

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            result_ = assert_not(filtered%matched())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_case_is_same(input) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use vegetables, only: input_t, result_t, test_item_t, assert_equals, fail

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            test_item = filtered%test()
            result_ = assert_equals(EXAMPLE_DESCRIPTION, test_item%description())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_not_matching(input) result(result_)
        use vegetables, only: input_t, result_t, assert_not, fail

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            result_ = assert_not(filtered%matched())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_is_same(input) result(result_)
        use example_collections_m, only: example_passing_collection
        use vegetables, only: input_t, result_t, test_item_t, assert_equals, fail

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_collection
        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            example_collection = example_passing_collection()
            test_item = filtered%test()
            result_ = assert_equals(example_collection%description(), test_item%description())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_single_case(input) result(result_)
        use vegetables, only: input_t, result_t, test_item_t, assert_equals, fail

        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            test_item = filtered%test()
            result_ = assert_equals(1, test_item%num_cases())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function constructor(input) result(filter_item_result_input)
        type(filter_item_result_t), intent(in) :: input
        type(filter_item_result_input_t) :: filter_item_result_input

        filter_item_result_input%input_ = input
    end function

    function input(self)
        class(filter_item_result_input_t), intent(in) :: self
        type(filter_item_result_t) :: input

        input = self%input_
    end function
end module
