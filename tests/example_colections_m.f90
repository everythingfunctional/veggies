module example_collections_m
    implicit none
    private

    character(len=*), parameter, public :: EXAMPLE_CASE_DESCRIPTION_1 = &
            "Example Case Description 1"
    character(len=*), parameter, public :: EXAMPLE_CASE_DESCRIPTION_2 = &
            "Example Case Description 2"
    character(len=*), parameter, public :: EXAMPLE_COLLECTION_DESCRIPTION = &
            "Example Collection Description"
    integer, parameter, public :: NUM_ASSERTS_IN_PASSING = 2
    integer, parameter, public :: NUM_CASES_IN_PASSING = 2

    public :: examplePassingCollection
contains
    function exampleTestCase1() result(test_case)
        use Vegetables_m, only: TestCase_t, it, succeed

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_CASE_DESCRIPTION_1, succeed)
    end function exampleTestCase1

    function exampleTestCase2() result(test_case)
        use Vegetables_m, only: TestCase_t, it, succeed

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_CASE_DESCRIPTION_2, succeed)
    end function exampleTestCase2

    function examplePassingCollection() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe

        type(TestCollection_t) :: test_collection

        test_collection = describe(EXAMPLE_COLLECTION_DESCRIPTION, &
                [exampleTestCase1(), exampleTestCase2()])
    end function examplePassingCollection
end module example_collections_m
