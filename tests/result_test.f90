module result_test
    implicit none
    private

    public :: test_result
contains
    function test_result() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(5)

        individual_tests(1) = it("Can tell whether they passed", checkPassed)
        individual_tests(2) = it( &
                "Can tell how many assertions there were", checkNumAsserts)
        individual_tests(3) = it(&
                "Can tell how many failing assertions there were", &
                checkNumFailingAsserts)
        individual_tests(4) = it( &
                "Verbose description includes all the messages", &
                checkVerboseIncludes)
        individual_tests(5) = it( &
                "Failure description only includes the failing messages", &
                checkFailureIncludes)
        tests = describe("Results", individual_tests)
    end function test_result

    function checkPassed() result(result_)
        use Vegetables_m, only: Result_t, assertNot, assertThat, fail, succeed

        type(Result_t) :: result_

        type(Result_t) :: failing_result
        type(Result_t) :: passing_result

        passing_result = succeed("Message")
        failing_result = fail("Message")

        result_ = &
                assertThat(passing_result%passed()) &
                .and.assertNot(failing_result%passed())
    end function checkPassed

    function checkNumAsserts() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, succeed

        type(Result_t) :: result_

        type(Result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.succeed("Second")

        result_ = assertEquals(2, multiple_asserts%numAsserts())
    end function checkNumAsserts

    function checkNumFailingAsserts() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, fail, succeed

        type(Result_t) :: result_

        type(Result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = assertEquals(1, multiple_asserts%numFailingAsserts())
    end function checkNumFailingAsserts

    function checkVerboseIncludes() result(result_)
        use Vegetables_m, only: Result_t, assertIncludes, fail, succeed

        type(Result_t) :: result_

        type(Result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = &
                assertIncludes( &
                        "First", multiple_asserts%verboseDescription(.false.))&
                .and.assertIncludes( &
                        "Second", multiple_asserts%verboseDescription(.false.))
    end function checkVerboseIncludes

    function checkFailureIncludes() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, assertIncludes, fail, succeed

        type(Result_t) :: result_

        type(Result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = &
                assertDoesntInclude( &
                        "First", multiple_asserts%failureDescription(.false.))&
                .and.assertIncludes( &
                        "Second", multiple_asserts%failureDescription(.false.))
    end function checkFailureIncludes
end module result_test
