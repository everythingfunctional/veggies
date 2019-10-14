module passing_collection_test
    implicit none
    private

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        use example_collections_m, only: examplePassingCollection
        use Helpers_m, only: TestItemInput_t, runTest
        use Vegetables_m, only: TestItem_t, Given, Then__, When

        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(1)
        type(TestItemInput_t) :: the_collection
        type(TestItem_t) :: individual_tests(9)

        the_collection%input = examplePassingCollection()
        individual_tests(1) = Then__("it knows it passed", checkCollectionPasses)
        individual_tests(2) = Then__("it knows how many cases there were", checkNumCases)
        individual_tests(3) = Then__("it has no failing cases", checkNumFailingCases)
        individual_tests(4) = Then__("it's verbose description includes the given description", checkVerboseTopDescription)
        individual_tests(5) = Then__( &
                "it's verbose description includes the individual case descriptions", &
                checkVerboseCaseDescriptions)
        individual_tests(6) = Then__("it's verbose description includes the assertion message", checkVerboseDescriptionAssertion)
        individual_tests(7) = Then__("it's failure description is empty", checkFailureDescriptionEmpty)
        individual_tests(8) = Then__("it knows how many asserts there were", checkNumAsserts)
        individual_tests(9) = Then__("it has no failing asserts", checkNumFailingAsserts)
        collection(1) = When("it is run", runTest, individual_tests)
        tests = Given("a passing test collection", the_collection, collection)
    end function test_passing_collection_behaviors

    function checkCollectionPasses(example_results) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertThat(example_results%input%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkCollectionPasses

    function checkNumCases(example_results) result(result_)
        use example_collections_m, only: NUM_CASES_IN_PASSING
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_CASES_IN_PASSING, example_results%input%numCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumCases

    function checkNumFailingCases(example_results) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(0, example_results%input%numFailingCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingCases

    function checkVerboseTopDescription(example_results) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseTopDescription

    function checkVerboseCaseDescriptions(example_results) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = &
                    assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%input%verboseDescription(.false.)) &
                    .and.assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseCaseDescriptions

    function checkVerboseDescriptionAssertion(example_results) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    SUCCESS_MESSAGE, &
                    example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseDescriptionAssertion

    function checkFailureDescriptionEmpty(example_results) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertEmpty, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEmpty(example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureDescriptionEmpty

    function checkNumAsserts(example_results) result(result_)
        use example_collections_m, only: NUM_ASSERTS_IN_PASSING
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, TestCollectionResult_t, assertEquals, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_ASSERTS_IN_PASSING, example_results%input%numAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_results) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(0, example_results%input%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingAsserts
end module passing_collection_test
