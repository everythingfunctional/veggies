module assert_empty_test
    implicit none
    private

    public :: test_assert_empty
contains
    function test_assert_empty() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with an empty character", checkPassForEmptyChars)
        individual_tests(2) = it("fails with a non empty character", checkFailsForNonemptyChars)
        tests = describe("assertEmpty", individual_tests)
    end function test_assert_empty

    function checkPassForEmptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("")

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription(.false.))
    end function checkPassForEmptyChars

    function checkFailsForNonemptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("Not Empty")

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription(.false.))
    end function checkFailsForNonemptyChars
end module assert_empty_test
