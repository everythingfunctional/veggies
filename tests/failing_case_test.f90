module failing_case_test
    implicit none
    private

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        use example_cases_m, only: exampleFailingTestCase, runCase
        use Vegetables_m, only: TestCase_t, TestItem_t, given, then_, when

        type(TestItem_t) :: test

        type(TestItem_t) :: collection(1)
        type(TestCase_t) :: example_case
        type(TestItem_t) :: individual_tests(11)

        example_case = exampleFailingTestCase()
        individual_tests(1) = then_("it knows it failed", checkCaseFails)
        individual_tests(2) = then_("it has 1 test case", checkNumCases)
        individual_tests(3) = then_("it has 1 failing case", checkNumFailingCases)
        individual_tests(4) = then_("it's verbose description includes the given description", checkVerboseForGivenDescription)
        individual_tests(5) = then_("it's verbose description includes the success message", checkVerboseForSuccessMessage)
        individual_tests(6) = then_("it's verbose description includes the failure message", checkVerboseForFailureMessage)
        individual_tests(7) = then_("it's failure description includes the given description", checkFailureForGivenDescription)
        individual_tests(8) = then_("it's failure description includes the failure message", checkFailureForFailureMessage)
        individual_tests(9) = then_("it's failure description doesn't include the success message", checkFailureNoSuccessMessage)
        individual_tests(10) = then_("it knows how many asserts there were", checkNumAsserts)
        individual_tests(11) = then_("it knows how many asserts failed", checkNumFailingAsserts)
        collection(1) = when("it is run", runCase, individual_tests)
        test = given("a failing test case", example_case, collection)
    end function test_failing_case_behaviors

    function checkCaseFails(example_result) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertNot, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertNot(example_result%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkCaseFails

    function checkNumCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumCases

    function checkNumFailingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numFailingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingCases

    function checkVerboseForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForGivenDescription

    function checkVerboseForSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForSuccessMessage

    function checkVerboseForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForFailureMessage

    function checkFailureForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureForGivenDescription

    function checkFailureForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureForFailureMessage

    function checkFailureNoSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertDoesntInclude, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertDoesntInclude(SUCCESS_MESSAGE, example_result%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureNoSuccessMessage

    function checkNumAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_result%numAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_FAILING_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals( &
                    NUM_FAILING_ASSERTS_IN_FAILING, example_result%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingAsserts
end module failing_case_test
