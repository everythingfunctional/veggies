module filter_test
    implicit none
    private

    public :: test_filter_case, test_filter_collection
contains
    function test_filter_case() result(tests)
        use example_cases_m, only: examplePassingTestCase
        use Vegetables_m, only: TestItem_t, TestCase_t, Given, Then_, When

        type(TestItem_t) :: tests

        type(TestCase_t) :: example_case

        example_case = examplePassingTestCase()
        tests = Given("a test case", example_case, &
                [When("it is filterd with a string it doesn't contain", filterCaseNotMatching, &
                        [Then_("it returns nothing", checkCaseForNothing)]), &
                When("it is filtered with a matching string", filterCaseMatching, &
                        [Then_("it returns itself", checkCaseIsSame)])])
    end function test_filter_case

    function test_filter_collection() result(tests)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: TestItem_t, TestCollection_t, Given, Then_, When

        type(TestItem_t) :: tests

        type(TestCollection_t) :: example_collection

        example_collection = examplePassingCollection()
        tests = Given("a test collection", example_collection, &
                [When("it is filtered with a string it doesn't contain", filterCollectionNotMatching, &
                        [Then_("it returns nothing", checkCollectionForNothing)])])
    end function test_filter_collection

    function filterCaseNotMatching(example_case) result(filtered)
        use example_cases_m, only: NOT_IN_DESCRIPTION
        use Vegetables_m, only: TestCase_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_case
        type(Transformed_t) :: filtered

        select type (example_case)
        type is (TestCase_t)
            filtered = Transformed(example_case%filter(NOT_IN_DESCRIPTION))
        class default
            filtered = Transformed(fail("Expected to get a TestCase_t"))
        end select
    end function filterCaseNotMatching

    function filterCollectionNotMatching(example_collection) result(filtered)
        use example_collections_m, only: NOT_IN_DESCRIPTIONS
        use Vegetables_m, only: TestCollection_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_collection
        type(Transformed_t) :: filtered

        select type (example_collection)
        type is (TestCollection_t)
            filtered = Transformed(example_collection%filter(NOT_IN_DESCRIPTIONS))
        class default
            filtered = Transformed(fail("Expected to get a TestCollection_t"))
        end select
    end function filterCollectionNotMatching

    function filterCaseMatching(example_case) result(filtered)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: TestCase_t, Transformed_t, fail, Transformed

        class(*), intent(in) :: example_case
        type(Transformed_t) :: filtered

        select type (example_case)
        type is (TestCase_t)
            filtered = Transformed(example_case%filter(EXAMPLE_DESCRIPTION))
        class default
            filtered = Transformed(fail("Expected to get a TestCase_t"))
        end select
    end function filterCaseMatching

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
end module filter_test
