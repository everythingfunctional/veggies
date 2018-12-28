module replace_newlines_test
    implicit none
    private

    public :: test_replace_newlines
contains
    function test_replace_newlines() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("replacing newlines in a string", &
                [it("returns the same string if there aren't any newlines", checkSameWithoutNewlines), &
                it("returns a string without newlines", checkNoNewlines), &
                it("returns '\n' for a string that is just a newline", checkForJustANewline)])
    end function test_replace_newlines

    function checkSameWithoutNewlines() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, replaceNewlines, toString

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Something Without Newlines"

        result_ = assertEquals(EXAMPLE_STRING, replaceNewlines(EXAMPLE_STRING))
    end function checkSameWithoutNewlines

    function checkNoNewlines() result(result_)
        use Vegetables_m, only: &
                Result_t, assertDoesntInclude, replaceNewlines, toString

        type(Result_t) :: result_

        character(len=*), parameter :: NEWLINE = NEW_LINE('A')
        character(len=*), parameter :: EXAMPLE_STRING = &
                "String" // NEWLINE // "With" // NEWLINE // "Newlines"

        result_ = assertDoesntInclude(NEWLINE, replaceNewlines(EXAMPLE_STRING))
    end function checkNoNewlines

    function checkForJustANewline() result(result_)
        use Vegetables_m, only: Result_t, assertEquals, replaceNewlines, toString

        type(Result_t) :: result_

        result_ = assertEquals("\n", replaceNewlines(NEW_LINE('A')))
    end function checkForJustANewline
end module replace_newlines_test
