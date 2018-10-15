module test_collection_result_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test_collection

        test_collection = describe("TestCollectionResult_t", &
                [it("Knows if it passed", passedCollectionKnowsIt)])
    end function testGenerator

    pure function passedCollectionKnowsIt() result(test_result)
        use Vegetables_m, only: TestCollectionResult_t, TestResult_t, assertThat

        type(TestResult_t) :: test_result
        type(TestCollectionResult_t) :: passed_collection

        passed_collection = getPassedCollection()

        test_result = assertThat(passed_collection%passed())
    end function passedCollectionKnowsIt

    pure function getPassedCollection() result(passed_collection)
        use Vegetables_m, only: TestCollection_t, TestCollectionResult_t

        type(TestCollectionResult_t) :: passed_collection

        type(TestCollection_t) :: passing_collection

        passing_collection = getPassingCollection()
        passed_collection = passing_collection%runCollection()
    end function getPassedCollection

    pure function getPassingCollection() result(passing_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: passing_collection

        passing_collection = describe("Passing Collection", &
                [it("Passes", passing)])
    end function getPassingCollection

    pure function passing() result(test_result)
        use Vegetables_m, only: TestResult_t, assertThat

        type(TestResult_t) :: test_result

        test_result = assertThat(.true.)
    end function passing
end module test_collection_result_test
