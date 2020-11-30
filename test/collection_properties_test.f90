module collection_properties_test
    implicit none
    private
    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use example_collections_m, only: example_passing_collection
        use helpers_m, only: test_item_input_t
        use vegetables, only: test_item_t, describe, it_

        type(test_item_t) :: test

        type(test_item_input_t) :: the_collection
        type(test_item_t) :: individual_tests(4)

        the_collection%input = example_passing_collection()
        individual_tests(1) = it_("can tell how many tests it has", checkNumCases)
        individual_tests(2) = it_("includes the given description", checkCollectionTopDescription)
        individual_tests(3) = it_("includes the individual test descriptions", checkCollectionDescriptions)
        individual_tests(4) = it_("Takes less than three times as long as the individual cases", checkSpeed)
        test = describe("A test collection", the_collection, individual_tests)
    end function

    pure function checkNumCases(example_collection) result(result_)
        use example_collections_m, only: NUM_CASES_IN_PASSING
        use helpers_m, only: test_item_input_t
        use vegetables, only: input_t, result_t, assert_equals, fail

        class(input_t), intent(in) :: example_collection
        type(result_t) :: result_

        select type (example_collection)
        class is (test_item_input_t)
            result_ = assert_equals(NUM_CASES_IN_PASSING, example_collection%input%num_cases())
        class default
            result_ = fail("Expected to get a test_collection_t")
        end select
    end function

    pure function checkCollectionTopDescription(example_collection) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use helpers_m, only: test_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_collection
        type(result_t) :: result_

        select type (example_collection)
        class is (test_item_input_t)
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, example_collection%input%description())
        class default
            result_ = fail("Expected to get a test_collection_t")
        end select
    end function

    pure function checkCollectionDescriptions(example_collection) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use helpers_m, only: test_item_input_t
        use vegetables, only: input_t, result_t, assert_includes, fail

        class(input_t), intent(in) :: example_collection
        type(result_t) :: result_

        select type (example_collection)
        class is (test_item_input_t)
            result_ = &
                    assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_1, example_collection%input%description()) &
                    .and.assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_2, example_collection%input%description())
        class default
            result_ = fail("Expected to get a test_collection_t")
        end select
    end function

    function checkSpeed(example_collection) result(result_)
        use example_collections_m, only: example_test_case_1, example_test_case_2
        use helpers_m, only: test_item_input_t
        use vegetables, only: &
                input_t, result_t, test_item_t, assert_faster_than, fail

        class(input_t), intent(in) :: example_collection
        type(result_t) :: result_

        type(test_item_t) :: internal_collection

        type(test_item_t) :: the_cases(3)

        the_cases(1) = example_test_case_1()
        the_cases(2) = example_test_case_2()
        the_cases(3) = example_test_case_2()

        select type (example_collection)
        type is (test_item_input_t)
            internal_collection = example_collection%input
            result_ = assert_faster_than(runCases, runCollection, 100)
        class default
            result_ = fail("Expected to get a test_collection_t")
        end select
    contains
        subroutine runCollection
            use vegetables, only: test_result_item_t

            integer :: i
            type(test_result_item_t) :: internal_result

            do i = 1, 100
                internal_result = internal_collection%run()
            end do
        end subroutine

        subroutine runCases
            use vegetables, only: test_result_item_t

            integer :: i
            type(test_result_item_t) :: the_results(3)

            do i = 1, 300
                the_results(1) = the_cases(1)%run()
                the_results(2) = the_cases(2)%run()
                the_results(3) = the_cases(3)%run()
            end do
        end subroutine
    end function
end module
