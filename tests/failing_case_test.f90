module failing_case_test
    implicit none
    private

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        use example_cases_m, only: exampleFailingTestCase
        use Helpers_m, only: TestItemInput_t, runTest
        use Vegetables_m, only: TestItem_t, Given, Then__, When

        type(TestItem_t) :: test

        type(TestItem_t) :: collection(1)
        type(TestItemInput_t) :: the_case
        type(TestItem_t) :: individual_tests(11)

        the_case%input = exampleFailingTestCase()
        individual_tests(1) = Then__("it knows it failed", checkCaseFails)
        individual_tests(2) = Then__("it has 1 test case", checkNumCases)
        individual_tests(3) = Then__("it has 1 failing case", checkNumFailingCases)
        individual_tests(4) = Then__("it's verbose description includes the given description", checkVerboseForGivenDescription)
        individual_tests(5) = Then__("it's verbose description includes the success message", checkVerboseForSuccessMessage)
        individual_tests(6) = Then__("it's verbose description includes the failure message", checkVerboseForFailureMessage)
        individual_tests(7) = Then__("it's failure description includes the given description", checkFailureForGivenDescription)
        individual_tests(8) = Then__("it's failure description includes the failure message", checkFailureForFailureMessage)
        individual_tests(9) = Then__("it's failure description doesn't include the success message", checkFailureNoSuccessMessage)
        individual_tests(10) = Then__("it knows how many asserts there were", checkNumAsserts)
        individual_tests(11) = Then__("it knows how many asserts failed", checkNumFailingAsserts)
        collection(1) = When("it is run", runTest, individual_tests)
        test = Given("a failing test case", the_case, collection)
    end function test_failing_case_behaviors

    function checkCaseFails(example_result) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertNot(example_result%input%passed())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkCaseFails

    function checkNumCases(example_result) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(1, example_result%input%numCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumCases

    function checkNumFailingCases(example_result) result(result_)
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(1, example_result%input%numFailingCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingCases

    function checkVerboseForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForGivenDescription

    function checkVerboseForSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForSuccessMessage

    function checkVerboseForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForFailureMessage

    function checkFailureForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForGivenDescription

    function checkFailureForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertIncludes, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForFailureMessage

    function checkFailureNoSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: &
                Input_t, Result_t, assertDoesntInclude, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertDoesntInclude(SUCCESS_MESSAGE, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureNoSuccessMessage

    function checkNumAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_FAILING
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_result%input%numAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_FAILING_ASSERTS_IN_FAILING
        use Helpers_m, only: TestResultItemInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals( &
                    NUM_FAILING_ASSERTS_IN_FAILING, example_result%input%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingAsserts
end module failing_case_test
