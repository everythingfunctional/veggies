module filter_test
    use example_cases_m, only: &
            examplePassingTestCase, EXAMPLE_DESCRIPTION, NOT_IN_DESCRIPTION
    use example_collections_m, only: &
            examplePassingCollection, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            NOT_IN_DESCRIPTIONS
    use Helpers_m, only: TestItemInput_t
    use iso_varying_string, only: var_str
    use Vegetables_m, only: &
            Input_t, &
            FilterItemResult_t, &
            Result_t, &
            TestItem_t, &
            TransformationFailure_t, &
            Transformed_t, &
            assertEquals, &
            assertNot, &
            fail, &
            Given, &
            Then__, &
            Transformed, &
            When

    implicit none
    private

    type, extends(Input_t) :: FilterItemResultInput_t
        type(FilterItemResult_t) :: input
    end type FilterItemResultInput_t

    public :: test_filter_case, test_filter_collection
contains
    function test_filter_case() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(2)
        type(TestItemInput_t) :: the_case
        type(TestItem_t) :: first(1)
        type(TestItem_t) :: second(1)

        the_case%input = examplePassingTestCase()
        first(1) = Then__("it doesn't match", checkCaseNotMatching)
        second(1) = Then__("it returns itself", checkCaseIsSame)
        collection(1) = When( &
                "it is filtered with a string it doesn't contain", &
                filterCaseNotMatching, &
                first)
        collection(2) = When( &
                "it is filtered with a matching string", &
                filterCaseMatching, &
                second)
        tests = Given("a test case", the_case, collection)
    end function test_filter_case

    function test_filter_collection() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(3)
        type(TestItemInput_t) :: the_collection
        type(TestItem_t) :: first(1)
        type(TestItem_t) :: second(1)
        type(TestItem_t) :: third(1)

        the_collection%input = examplePassingCollection()
        first(1) = Then__("it doesn't match", checkCollectionNotMatching)
        second(1) = Then__("it returns itself", checkCollectionIsSame)
        third(1) = Then__("it returns a collection with only that case", checkCollectionSingleCase)
        collection(1) = When( &
                "it is filtered with a string it doesn't contain", &
                filterCollectionNotMatching, &
                first)
        collection(2) = When( &
                "it is filtered with a string matching its description", &
                filterCollectionMatchingDescription, &
                second)
        collection(3) = When( &
                "it is filtered with a string matching only 1 of its cases", &
                filterCollectionMatchingCase, &
                third)
        tests = Given("a test collection", the_collection, collection)
    end function test_filter_collection

    pure function filterCaseNotMatching(example_case) result(filtered)
        class(Input_t), intent(in) :: example_case
        type(Transformed_t) :: filtered

        type(TransformationFailure_t) :: failure
        type(FilterItemResultInput_t) :: the_result

        select type (example_case)
        type is (TestItemInput_t)
            the_result%input = example_case%input%filter(var_str(NOT_IN_DESCRIPTION))
            filtered = Transformed(the_result)
        class default
        failure%result_ = fail("Expected to get a TestItemInput_t")
        filtered = Transformed(failure)
        end select
    end function filterCaseNotMatching

    pure function filterCaseMatching(example_case) result(filtered)
        class(Input_t), intent(in) :: example_case
        type(Transformed_t) :: filtered

        type(TransformationFailure_t) :: failure
        type(FilterItemResultInput_t) :: the_result

        select type (example_case)
        type is (TestItemInput_t)
            the_result%input = example_case%input%filter(var_str(EXAMPLE_DESCRIPTION))
            filtered = Transformed(the_result)
        class default
        failure%result_ = fail("Expected to get a TestItemInput_t")
        filtered = Transformed(failure)
        end select
    end function filterCaseMatching

    pure function filterCollectionNotMatching(example_collection) result(filtered)
        class(Input_t), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        type(TransformationFailure_t) :: failure
        type(FilterItemResultInput_t) :: the_result

        select type (example_collection)
        type is (TestItemInput_t)
            the_result%input = example_collection%input%filter(var_str(NOT_IN_DESCRIPTIONS))
            filtered = Transformed(the_result)
        class default
        failure%result_ = fail("Expected to get a TestItemInput_t")
        filtered = Transformed(failure)
        end select
    end function filterCollectionNotMatching

    pure function filterCollectionMatchingDescription(example_collection) result(filtered)
        class(Input_t), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        type(TransformationFailure_t) :: failure
        type(FilterItemResultInput_t) :: the_result

        select type (example_collection)
        type is (TestItemInput_t)
            the_result%input = example_collection%input%filter(var_str(EXAMPLE_COLLECTION_DESCRIPTION))
            filtered = Transformed(the_result)
        class default
        failure%result_ = fail("Expected to get a TestItemInput_t")
        filtered = Transformed(failure)
        end select
    end function filterCollectionMatchingDescription

    pure function filterCollectionMatchingCase(example_collection) result(filtered)
        class(Input_t), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        type(TransformationFailure_t) :: failure
        type(FilterItemResultInput_t) :: the_result

        select type (example_collection)
        type is (TestItemInput_t)
            the_result%input = example_collection%input%filter(var_str(EXAMPLE_CASE_DESCRIPTION_1))
            filtered = Transformed(the_result)
        class default
        failure%result_ = fail("Expected to get a TestItemInput_t")
        filtered = Transformed(failure)
        end select
    end function filterCollectionMatchingCase

    pure function checkCaseNotMatching(filtered) result(result_)
        class(Input_t), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (FilterItemResultInput_t)
            result_ = assertNot(filtered%input%matched)
        class default
            result_ = fail("Expected to get FilterItemResultInput_t")
        end select
    end function checkCaseNotMatching

    pure function checkCaseIsSame(filtered) result(result_)
        class(Input_t), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (FilterItemResultInput_t)
            result_ = assertEquals(EXAMPLE_DESCRIPTION, filtered%input%test%description())
        class default
            result_ = fail("Expected to get FilterItemResultInput_t")
        end select
    end function checkCaseIsSame

    pure function checkCollectionNotMatching(filtered) result(result_)
        class(Input_t), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (FilterItemResultInput_t)
            result_ = assertNot(filtered%input%matched)
        class default
            result_ = fail("Expected to get FilterItemResultInput_t")
        end select
    end function checkCollectionNotMatching

    function checkCollectionIsSame(filtered) result(result_)
        class(Input_t), intent(in) :: filtered
        type(Result_t) :: result_

        type(TestItem_t) :: example_collection

        select type (filtered)
        type is (FilterItemResultInput_t)
            example_collection = examplePassingCollection()
            result_ = assertEquals(example_collection%description(), filtered%input%test%description())
        class default
            result_ = fail("Expected to get FilterItemResultInput_t")
        end select
    end function checkCollectionIsSame

    pure function checkCollectionSingleCase(filtered) result(result_)
        class(Input_t), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (FilterItemResultInput_t)
            result_ = assertEquals(1, filtered%input%test%numCases())
        class default
            result_ = fail("Expected to get FilterItemResultInput_t")
        end select
    end function checkCollectionSingleCase
end module filter_test
