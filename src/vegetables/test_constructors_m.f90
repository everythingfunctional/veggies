module vegetables_test_constructors_m
    implicit none
    private
    public :: describe, given, it, it_, test_that, then_, then__, when

    interface describe
        module procedure describe_basic_c
        module procedure describe_basic_s
        module procedure describe_with_input_c
        module procedure describe_with_input_s
    end interface

    interface given
        module procedure given_basic_c
        module procedure given_basic_s
        module procedure given_with_input_c
        module procedure given_with_input_s
    end interface

    interface it
        module procedure it_basic_c
        module procedure it_basic_s
        module procedure it_with_examples_c
        module procedure it_with_examples_s
        module procedure it_with_generator_c
        module procedure it_with_generator_s
    end interface

    interface it_
        module procedure it_input_c
        module procedure it_input_s
    end interface

    interface then_
        module procedure then_basic_c
        module procedure then_basic_s
    end interface

    interface then__
        module procedure then_input_c
        module procedure then_input_s
    end interface

    interface when
        module procedure when_basic_c
        module procedure when_basic_s
        module procedure when_with_transformer_c
        module procedure when_with_transformer_s
    end interface
contains
    function describe_basic_c(description, tests) result(item)
        use iso_varying_string, only: var_str
        use vegetables_simple_test_collection_m, only: simple_test_collection
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection( &
                var_str(description), tests))
    end function

    function describe_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_simple_test_collection_m, only: simple_test_collection
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection( &
                description, tests))
    end function

    function describe_with_input_c(description, input, tests) result(item)
        use iso_varying_string, only: var_str
        use vegetables_input_m, only: input_t
        use vegetables_test_collection_with_input_m, only: &
                test_collection_with_input_t
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                var_str(description), input, tests))
    end function

    function describe_with_input_s(description, input, tests) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_input_m, only: input_t
        use vegetables_test_collection_with_input_m, only: &
                test_collection_with_input_t
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                description, input, tests))
    end function

    function given_basic_c(description, tests) result(item)
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_with_input_c(description, input, tests) result(item)
        use vegetables_input_m, only: input_t
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function given_with_input_s(description, input, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_input_m, only: input_t
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function it_basic_c(description, test) result(item)
        use iso_varying_string, only: var_str
        use vegetables_simple_test_case_m, only: simple_test_case_t
        use vegetables_test_interfaces_m, only: simple_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(var_str(description), test))
    end function

    function it_basic_s(description, test) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_simple_test_case_m, only: simple_test_case_t
        use vegetables_test_interfaces_m, only: simple_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(description, test))
    end function

    function it_input_c(description, test) result(item)
        use iso_varying_string, only: var_str
        use vegetables_input_test_case_m, only: input_test_case_t
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(var_str(description), test))
    end function

    function it_input_s(description, test) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_input_test_case_m, only: input_test_case_t
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(description, test))
    end function

    function it_with_examples_c(description, examples, test) result(item)
        use iso_varying_string, only: var_str
        use vegetables_example_m, only: example_t
        use vegetables_test_case_with_examples_m, only: test_case_with_examples
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples( &
                var_str(description), examples, test))
    end function

    function it_with_examples_s(description, examples, test) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_example_m, only: example_t
        use vegetables_test_case_with_examples_m, only: test_case_with_examples
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples( &
                description, examples, test))
    end function

    function it_with_generator_c(description, generator, test) result(item)
        use iso_varying_string, only: var_str
        use vegetables_generator_m, only: generator_t
        use vegetables_test_case_with_generator_m, only: test_case_with_generator
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator( &
                var_str(description), generator, test))
    end function

    function it_with_generator_s(description, generator, test) result(item)
        use iso_varying_string, only: varying_string
        use vegetables_generator_m, only: generator_t
        use vegetables_test_case_with_generator_m, only: test_case_with_generator
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator( &
                description, generator, test))
    end function

    function test_that(tests) result(item)
        use vegetables_test_item_m, only: test_item_t

        type(test_item_t) :: tests(:)
        type(test_item_t) :: item

        item = describe("Test that", tests)
    end function

    function then_basic_c(description, test) result(item)
        use vegetables_test_interfaces_m, only: simple_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_basic_s(description, test) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_test_interfaces_m, only: simple_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_input_c(description, test) result(item)
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function then_input_s(description, test) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_test_interfaces_m, only: input_test_i
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function when_basic_c(description, tests) result(item)
        use vegetables_test_item_m, only: test_item_t

        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_basic_s(description, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_test_item_m, only: test_item_t

        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_with_transformer_c(description, transformer, tests) result(item)
        use iso_varying_string, only: var_str
        use vegetables_test_interfaces_m, only: transformer_i
        use vegetables_test_item_m, only: test_item_t
        use vegetables_transforming_test_collection_m, only: &
                transforming_test_collection

        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection( &
                var_str("When " // description), transformer, tests))
    end function

    function when_with_transformer_s(description, transformer, tests) result(item)
        use iso_varying_string, only: varying_string, operator(//)
        use vegetables_test_interfaces_m, only: transformer_i
        use vegetables_test_item_m, only: test_item_t
        use vegetables_transforming_test_collection_m, only: &
                transforming_test_collection

        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection( &
                "When " // description, transformer, tests))
    end function
end module
