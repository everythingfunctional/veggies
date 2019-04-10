module filter_test
    implicit none
    private

    public :: test_filter_case, test_filter_collection
contains
    function test_filter_case() result(tests)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: TestItem_t, TestCase_t, Given, Then_, When

        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(2)
        type(TestCase_t) :: example_case
        type(TestItem_t) :: first(1)
        type(TestItem_t) :: second(1)

        example_case = examplePassingTestCase()
        first(1) = Then_("it returns nothing", checkCaseForNothing)
        second(1) = Then_("it returns itself", checkCaseIsSame)
        collection(1) = When( &
                "it is filterd with a string it doesn't contain", &
                filterCaseNotMatching, &
                first)
        collection(2) = When( &
                "it is filtered with a matching string", &
                filterCaseMatching, &
                second)
        tests = Given("a test case", example_case, collection)
    end function test_filter_case

    function test_filter_collection() result(tests)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: TestItem_t, TestCollection_t, Given, Then_, When

        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(3)
        type(TestCollection_t) :: example_collection
        type(TestItem_t) :: first(1)
        type(TestItem_t) :: second(1)
        type(TestItem_t) :: third(1)

        example_collection = examplePassingCollection()
        first(1) = Then_("it returns nothing", checkCollectionForNothing)
        second(1) = Then_("it returns itself", checkCollectionIsSame)
        third(1) = Then_("it returns a collection with only that case", checkCollectionSingleCase)
        collection(1) = When( &
                "it is filtered with a string it doesn't contain", &
                filterCollectionNotMatching, &
                first)
        collection(2) = When( &
                "it is filtered with a string matching it's description", &
                filterCollectionMatchingDescription, &
                second)
        collection(3) = When( &
                "it is filtered with a string matching only 1 of its cases", &
                filterCollectionMatchingCase, &
                third)
        tests = Given("a test collection", example_collection, collection)
    end function test_filter_collection

    function filterCaseNotMatching(example_case) result(filtered)
        use example_cases_m, only: NOT_IN_DESCRIPTION
        use Vegetables_m, only: Maybe_t, TestCase_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_case
        type(Transformed_t) :: filtered

        class(Maybe_t), allocatable :: maybe

        select type (example_case)
        type is (TestCase_t)
            allocate(maybe, source = example_case%filter(NOT_IN_DESCRIPTION))
            filtered = Transformed(maybe)
        class default
            filtered = Transformed(fail("Expected to get a TestCase_t"))
        end select
    end function filterCaseNotMatching

    function filterCollectionNotMatching(example_collection) result(filtered)
        use example_collections_m, only: NOT_IN_DESCRIPTIONS
        use Vegetables_m, only: Maybe_t, TestCollection_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        class(Maybe_t), allocatable :: maybe

        select type (example_collection)
        type is (TestCollection_t)
            allocate(maybe, source = example_collection%filter(NOT_IN_DESCRIPTIONS))
            filtered = Transformed(maybe)
        class default
            filtered = Transformed(fail("Expected to get a TestCollection_t"))
        end select
    end function filterCollectionNotMatching

    function filterCaseMatching(example_case) result(filtered)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Maybe_t, TestCase_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_case
        type(Transformed_t) :: filtered

        class(Maybe_t), allocatable :: maybe

        select type (example_case)
        type is (TestCase_t)
            allocate(maybe, source = example_case%filter(EXAMPLE_DESCRIPTION))
            filtered = Transformed(maybe)
        class default
            filtered = Transformed(fail("Expected to get a TestCase_t"))
        end select
    end function filterCaseMatching

    function filterCollectionMatchingDescription(example_collection) result(filtered)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: Maybe_t, TestCollection_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        class(Maybe_t), allocatable :: maybe

        select type (example_collection)
        type is (TestCollection_t)
            allocate(maybe, source = example_collection%filter(EXAMPLE_COLLECTION_DESCRIPTION))
            filtered = Transformed(maybe)
        class default
            filtered = Transformed(fail("Expected to get a TestCollection_t"))
        end select
    end function filterCollectionMatchingDescription

    function filterCollectionMatchingCase(example_collection) result(filtered)
        use example_collections_m, only: EXAMPLE_CASE_DESCRIPTION_1
        use Vegetables_m, only: Maybe_t, TestCollection_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        class(Maybe_t), allocatable :: maybe

        select type (example_collection)
        type is (TestCollection_t)
            allocate(maybe, source = example_collection%filter(EXAMPLE_CASE_DESCRIPTION_1))
            filtered = Transformed(maybe)
        class default
            filtered = Transformed(fail("Expected to get a TestCollection_t"))
        end select
    end function filterCollectionMatchingCase

    function checkCaseForNothing(filtered) result(result_)
        use Vegetables_m, only: Result_t, Nothing_t, fail, succeed

        class(*), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (Nothing_t)
            result_ = succeed("Got Nothing_t")
        class default
            result_ = fail("Expected to get Nothing_t")
        end select
    end function checkCaseForNothing

    function checkCollectionForNothing(filtered) result(result_)
        use Vegetables_m, only: Result_t, Nothing_t, fail, succeed

        class(*), intent(in) :: filtered
        type(Result_t) :: result_

        select type (filtered)
        type is (Nothing_t)
            result_ = succeed("Got Nothing_t")
        class default
            result_ = fail("Expected to get Nothing_t")
        end select
    end function checkCollectionForNothing

    function checkCaseIsSame(filtered) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, JustTestCase_t, TestCase_t, assertEquals, fail

        class(*), intent(in) :: filtered
        type(Result_t) :: result_

        type(TestCase_t) :: test_case

        select type (filtered)
        type is (JustTestCase_t)
            test_case = filtered%getValue()
            result_ = assertEquals(EXAMPLE_DESCRIPTION, test_case%description())
        class default
            result_ = fail("Expected to get JustTestCase_t")
        end select
    end function checkCaseIsSame

    function checkCollectionIsSame(filtered) result(result_)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: &
                Result_t, &
                JustTestCollection_t, &
                TestCollection_t, &
                assertEquals, &
                fail

        class(*), intent(in) :: filtered
        type(Result_t) :: result_

        type(TestCollection_t) :: filtered_collection
        type(TestCollection_t) :: example_collection

        select type (filtered)
        type is (JustTestCollection_t)
            example_collection = examplePassingCollection()
            filtered_collection = filtered%getValue()
            result_ = assertEquals(example_collection%description(), filtered_collection%description())
        class default
            result_ = fail("Expected to get JustTestCollection_t")
        end select
    end function checkCollectionIsSame

    function checkCollectionSingleCase(filtered) result(result_)
        use Vegetables_m, only: &
                Result_t, &
                JustTestCollection_t, &
                TestCollection_t, &
                assertEquals, &
                fail

        class(*), intent(in) :: filtered
        type(Result_t) :: result_

        type(TestCollection_t) :: filtered_collection

        select type (filtered)
        type is (JustTestCollection_t)
            filtered_collection = filtered%getValue()
            result_ = assertEquals(1, filtered_collection%numCases())
        class default
            result_ = fail("Expected to get JustTestCollection_t")
        end select
    end function checkCollectionSingleCase
end module filter_test
