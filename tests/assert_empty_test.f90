module assert_empty_test
    implicit none
    private

    public :: test_assert_empty
contains
    function test_assert_empty() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEmpty", &
                [it("passes with an empty character", checkPassForEmptyChars), &
                it("fails with a non empty character", checkFailsForNonemptyChars)])
    end function test_assert_empty

    function checkPassForEmptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("")

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
    end function checkPassForEmptyChars

    function checkFailsForNonemptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("Not Empty")

        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailsForNonemptyChars
end module assert_empty_test
