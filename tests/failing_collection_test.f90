module failing_collection_test
    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        use Vegetables_m, only: TestItem_t, given, then, when

        type(TestItem_t) :: tests

        tests = given("a failing test collection", &
                [when("it is run", &
                        [then("it knows it failed", checkCollectionFails), &
                        then("it knows how many cases there were", checkNumCases), &
                        then("it knows how many cases passed", checkNumPassingCases), &
                        then("it knows how many cases failed", checkNumFailingCases), &
                        then("it's verbose description includes the given description", checkVerboseTopDescription), &
                        then("it's verbose description includes the individual case descriptions", checkVerboseCaseDescriptions), &
                        then("it's verbose description includes the failure message", checkVerboseForFailureMessage), &
                        then("it's verbose description includes the success message", checkVerboseForSuccessMessage), &
                        then("it's failure description includes the given description", checkFailureForTopDescription), &
                        then("it's failure description includes the failing case description", checkFailureCaseDescription), &
                        then( &
                                "it's failure description does not include the passing case descriptions", &
                                checkFailureNoPassingDescriptions), &
                        then("it's failure description includes the failure message", checkFailureForMessage), &
                        then( &
                                "it's failure description does not include the success message", &
                                checkFailureNoSuccessMessage), &
                        then("it's failure description does not include blank lines", checkFailureNoBlankLines), &
                        then("it knows how many asserts there were", checkNumAsserts), &
                        then("it knows how many asserts failed", checkNumFailingAsserts), &
                        then("it knows how many asserts passed", checkNumPassingAsserts)])])
    end function test_failing_collection_behaviors

    function checkCollectionFails() result(result_)
        use example_collections_m, only: exampleFailingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertNot, &
                assertThat

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertThat(test_results%failed()).and.assertNot(test_results%passed())
    end function checkCollectionFails

    function checkNumCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_CASES_IN_FAILING, test_results%numCases())
    end function checkNumCases

    function checkNumPassingCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_PASSING_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_PASSING_CASES_IN_FAILING, test_results%numPassingCases())
    end function checkNumPassingCases

    function checkNumFailingCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_FAILING_CASES
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_FAILING_CASES, test_results%numFailingCases())
    end function checkNumFailingCases

    function checkVerboseTopDescription() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                EXAMPLE_COLLECTION_DESCRIPTION, &
                test_results%verboseDescription())
    end function checkVerboseTopDescription

    function checkVerboseCaseDescriptions() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, &
                EXAMPLE_CASE_DESCRIPTION_1, &
                EXAMPLE_CASE_DESCRIPTION_2, &
                EXAMPLE_FAILING_CASE_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = &
                assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_1, &
                        test_results%verboseDescription()) &
                .and.assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_2, &
                        test_results%verboseDescription()) &
                .and.assertIncludes( &
                        EXAMPLE_FAILING_CASE_DESCRIPTION, &
                        test_results%verboseDescription())
    end function checkVerboseCaseDescriptions

    function checkVerboseForFailureMessage() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, &
                FAILURE_MESSAGE
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                FAILURE_MESSAGE, &
                test_results%verboseDescription())
    end function checkVerboseForFailureMessage

    function checkVerboseForSuccessMessage() result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use example_collections_m, only: exampleFailingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                SUCCESS_MESSAGE, &
                test_results%verboseDescription())
    end function checkVerboseForSuccessMessage

    function checkFailureForTopDescription() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                EXAMPLE_COLLECTION_DESCRIPTION, &
                test_results%failureDescription())
    end function checkFailureForTopDescription

    function checkFailureCaseDescription() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, EXAMPLE_FAILING_CASE_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                EXAMPLE_FAILING_CASE_DESCRIPTION, &
                test_results%failureDescription())
    end function checkFailureCaseDescription

    function checkFailureNoPassingDescriptions() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, &
                EXAMPLE_CASE_DESCRIPTION_1, &
                EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertDoesntInclude

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = &
                assertDoesntInclude( &
                        EXAMPLE_CASE_DESCRIPTION_1, &
                        test_results%failureDescription()) &
                .and.assertDoesntInclude( &
                        EXAMPLE_CASE_DESCRIPTION_2, &
                        test_results%failureDescription())
    end function checkFailureNoPassingDescriptions

    function checkFailureForMessage() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, FAILURE_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertIncludes( &
                FAILURE_MESSAGE, &
                test_results%failureDescription())
    end function checkFailureForMessage

    function checkFailureNoSuccessMessage() result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use example_collections_m, only: exampleFailingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertDoesntInclude

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertDoesntInclude( &
                SUCCESS_MESSAGE, &
                test_results%failureDescription())
    end function checkFailureNoSuccessMessage

    function checkFailureNoBlankLines() result(result_)
        use example_collections_m, only: exampleFailingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertDoesntInclude

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertDoesntInclude( &
                NEW_LINE('A') // NEW_LINE('A'), &
                test_results%failureDescription())
    end function checkFailureNoBlankLines

    function checkNumAsserts() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_ASSERTS_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_ASSERTS_IN_FAILING, test_results%numAsserts())
    end function checkNumAsserts

    function checkNumFailingAsserts() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_FAILING_ASSERTS
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_FAILING_ASSERTS, test_results%numFailingAsserts())
    end function checkNumFailingAsserts

    function checkNumPassingAsserts() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_PASSING_ASSERTS_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals( &
                NUM_PASSING_ASSERTS_IN_FAILING, test_results%numPassingAsserts())
    end function checkNumPassingAsserts
end module failing_collection_test
