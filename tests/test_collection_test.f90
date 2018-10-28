module test_collection_test
    implicit none
    private

    character(len=*), parameter :: EXAMPLE_DESCRIPTION1 = "Example Description 1"
    character(len=*), parameter :: EXAMPLE_DESCRIPTION2 = "Example Description 2"
    character(len=*), parameter :: EXAMPLE_DESCRIPTION3 = "Example Description 3"

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use Vegetables_m, only: TestCollection_t, Describe, It

        type(TestCollection_t) :: test

        test = Describe("A test collection", &
                [It("can tell how many tests it has", checkNumCases)])
    end function test_collection_properties

    function checkNumCases() result(result_)
        use Vegetables_m, only: Result_t, TestCollection_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = exampleTestCollection()
        result_ = assertEquals(2, test_collection%numCases())
    end function checkNumCases

    function exampleTestCase1() result(test_case)
        use Vegetables_m, only: TestCase_t, It, succeed

        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION1, succeed)
    end function exampleTestCase1

    function exampleTestCase2() result(test_case)
        use Vegetables_m, only: TestCase_t, It, succeed

        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION2, succeed)
    end function exampleTestCase2

    function exampleTestCollection() result(test_collection)
        use Vegetables_m, only: TestCollection_t, Describe

        type(TestCollection_t) :: test_collection

        test_collection = Describe(EXAMPLE_DESCRIPTION3, &
                [exampleTestCase1(), exampleTestCase2()])
    end function exampleTestCollection
end module test_collection_test
