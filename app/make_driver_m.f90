module make_driver_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: make_driver

    type :: test_info_t
        type(varying_string) :: module_name
        type(varying_string), allocatable :: function_names(:)
    end type
contains
    subroutine make_driver(driver_file, test_files)
        use iso_varying_string, only: varying_string, char, put

        type(varying_string), intent(in) :: driver_file
        type(varying_string), intent(in) :: test_files(:)

        integer :: file_unit
        type(varying_string) :: program_
        type(test_info_t) :: test_infos(size(test_files))

        call get_test_info(test_files, test_infos)
        program_ = make_program(take_file_name(drop_extension(driver_file)), test_infos)
        open(newunit = file_unit, file = char(driver_file), action = "WRITE", status = "REPLACE")
        call put(file_unit, program_)
        close(file_unit)
    end subroutine

    subroutine get_test_info(filenames, test_infos)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: filenames(:)
        type(test_info_t), intent(out) :: test_infos(size(filenames))

        integer :: i

        do i = 1, size(filenames)
            call get_individual_test_info(filenames(i), test_infos(i))
        end do
    end subroutine

    subroutine get_individual_test_info(filename, test_info)
        use iso_varying_string, only: varying_string

        type(varying_string), intent(in) :: filename
        type(test_info_t), intent(out) :: test_info

        test_info%module_name = take_file_name(drop_extension(filename))
        call scan_test_file(filename, test_info%function_names)
    end subroutine

    pure function take_file_name(filename) result(just_file)
        use iso_varying_string, only: varying_string, extract, index

        type(varying_string), intent(in) :: filename
        type(varying_string) :: just_file

        just_file = extract(filename, start = index(filename, "/", back=.true.) + 1)
    end function

    pure function drop_extension(filename) result(without_extension)
        use iso_varying_string, only: varying_string, extract, index

        type(varying_string), intent(in) :: filename
        type(varying_string) :: without_extension

        integer :: slash_position
        integer :: dot_position

        slash_position = index(filename, "/", back=.true.)
        dot_position = index(filename, ".", back=.true.)

        if (dot_position <= slash_position) then
            without_extension = filename
        else
            without_extension = extract(filename, finish = dot_position-1)
        end if
    end function

    subroutine scan_test_file(filename, function_names)
        use iso_varying_string, only: varying_string
        use parff, only: parse_result_t, parsed_string_t
        use strff, only: read_file_lines

        type(varying_string), intent(in) :: filename
        type(varying_string), allocatable, intent(out) :: function_names(:)

        logical, allocatable :: function_mask(:)
        type(parse_result_t), allocatable :: function_name_results(:)
        integer :: i
        type(varying_string), allocatable :: lines(:)
        type(parse_result_t), allocatable :: maybe_function_names(:)
        integer :: num_function_names

        allocate(lines(0))
        lines = read_file_lines(filename)
        allocate(maybe_function_names(size(lines)))
        maybe_function_names = parse_line(lines)
        num_function_names = count(maybe_function_names%ok)
        allocate(function_name_results(num_function_names))
        allocate(function_mask(size(lines)))
        function_mask = maybe_function_names%ok
        function_name_results = pack(maybe_function_names, function_mask)
        allocate(function_names(num_function_names))
        do i = 1, num_function_names
            select type (name => function_name_results(i)%parsed)
            type is (parsed_string_t)
                function_names(i) = name%value_
            end select
        end do
    end subroutine

    elemental function parse_line(line) result(maybe_name)
        use iso_varying_string, only: varying_string
        use parff, only: parse_result_t, parse_with

        type(varying_string), intent(in) :: line
        type(parse_result_t) :: maybe_name

        maybe_name = parse_with(parse_test_function_name, line)
    end function

    pure function parse_test_function_name(the_state) result(the_result)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_string_t, &
                parser_output_t, &
                state_t, &
                drop_then, &
                empty_error, &
                message, &
                then_drop
        use strff, only: operator(.startswith.)

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = then_drop( &
                drop_then( &
                        drop_then( &
                                drop_then(parse_at_least_one_white_space, parse_function, the_state), &
                                parse_at_least_one_white_space), &
                        parse_valid_identifier), &
                parse_space_or_open_paren)
        if (the_result%ok) then
            select type (the_name => the_result%parsed)
            type is (parsed_string_t)
                if (.not. (the_name%value_.startswith."test_")) then
                    the_result = empty_error(message( &
                            the_state%position, the_name%value_, [var_str("test_*")]))
                end if
            end select
        end if
    end function

    pure function parse_function(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, parse_string

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = parse_string("function", the_state)
    end function

    pure function parse_space_or_open_paren(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, either

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = either(parse_at_least_one_white_space, parse_open_paren, the_state)
    end function

    pure function parse_open_paren(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = parse_char("(", the_state)
    end function

    pure function parse_at_least_one_white_space(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, many1

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = many1(parse_white_space, the_state)
    end function

    pure function parse_valid_identifier(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, sequence

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = sequence( &
                parse_valid_first_character, &
                then_parse_valid_identifiers, &
                the_state)
    contains
        pure function then_parse_valid_identifiers(previous, state_) result(result_)
            use iso_varying_string, only: assignment(=), operator(//), var_str
            use parff, only: &
                    parsed_character_t, &
                    parsed_items_t, &
                    parsed_string_t, &
                    parsed_value_t, &
                    parser_output_t, &
                    state_t, &
                    consumed_ok, &
                    many, &
                    message

            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            integer :: i
            type(parsed_string_t) :: parsed_identifier

            select type (previous)
            type is (parsed_character_t)
                parsed_identifier%value_ = previous%value_
                result_ = many(parse_valid_identifier_character, state_)
                if (result_%empty) then
                    result_ = consumed_ok( &
                            parsed_identifier, &
                            state_%input, &
                            state_%position, &
                            message( &
                                    state_%position, &
                                    var_str(""), &
                                    [varying_string::]))
                else
                    select type (results => result_%parsed)
                    type is (parsed_items_t)
                        do i = 1, size(results%items)
                            select type (next_char => results%items(i)%item)
                            type is (parsed_character_t)
                                parsed_identifier%value_ = &
                                        parsed_identifier%value_ &
                                        // next_char%value_
                            end select
                        end do
                    end select
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = parsed_identifier)
                end if
            end select
        end function
    end function

    pure function parse_valid_first_character(the_state) result(the_result)
        use parff, only: parser_output_t, state_t

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = parse_alphabet(the_state)
    end function

    pure function parse_valid_identifier_character(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, either

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = either(parse_alphabet, parse_non_letter, the_state)
    contains
        pure function parse_non_letter(state_) result(result_)
            use parff, only: parser_output_t, state_t, parse_digit

            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_digit, parse_underscore, state_)
        end function
    end function

    pure function parse_alphabet(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, with_label

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("letter", the_parser, the_state)
    contains
        pure function the_parser(state_) result(result_)
            use parff, only: parser_output_t, state_t, satisfy

            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
        end function

        pure function the_matcher(char_) result(matches)
            use strff, only: operator(.includes.)

            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=*), parameter :: LETTER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

            matches = LETTER.includes.char_
        end function
    end function

    pure function parse_underscore(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = parse_char("_", the_state)
    end function

    pure function parse_white_space(the_state) result(the_result)
        use parff, only: parser_output_t, state_t, with_label

        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("whitespace", the_parser, the_state)
    contains
        pure function the_parser(state_) result(result_)
            use parff, only: parser_output_t, state_t, satisfy

            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
        end function

        pure function the_matcher(char_) result(matches)
            use strff, only: operator(.includes.)

            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=1), parameter :: TAB = char(9)
            character(len=1), parameter :: SPACE = char(32)
            character(len=*), parameter :: WHITESPACE = TAB // SPACE

            matches = WHITESPACE.includes.char_
        end function
    end function

    pure function make_program(driver_name, test_infos) result(program_)
        use iso_varying_string, only: varying_string, operator(//), var_str
        use strff, only: join, NEWLINE

        type(varying_string), intent(in) :: driver_name
        type(test_info_t), intent(in) :: test_infos(:)
        type(varying_string) :: program_

        type(varying_string), allocatable :: test_array(:)
        type(varying_string) :: use_statements(size(test_infos))

        call make_test_array(test_infos, test_array)
        use_statements = make_use_statements(test_infos)

        program_ = join( &
                [var_str("! Generated by make_vegetable_driver. DO NOT EDIT"), &
                "program " // driver_name, &
                var_str("    implicit none"), &
                var_str(""), &
                var_str("    call run()"), &
                var_str("contains"), &
                var_str("    subroutine run()"), &
                use_statements, &
                var_str("        use vegetables, only: test_item_t, test_that, run_tests"), &
                var_str(""), &
                var_str("        type(test_item_t) :: tests"), &
                test_array, &
                var_str(""), &
                var_str("        call run_tests(tests)"), &
                var_str("    end subroutine"), &
                var_str("end program")], &
                NEWLINE)
    end function

    pure function make_use_statements(test_infos) result(use_statements)
        use iso_varying_string, only: varying_string

        type(test_info_t), intent(in) :: test_infos(:)
        type(varying_string) :: use_statements(size(test_infos))

        integer :: i

        do i = 1, size(test_infos)
            use_statements(i) = make_use_statement(test_infos(i))
        end do
    end function

    pure function make_use_statement(test_info) result(use_statement)
        use iso_varying_string, only: varying_string, assignment(=), operator(//)
        use strff, only: join, NEWLINE

        type(test_info_t), intent(in) :: test_info
        type(varying_string) :: use_statement

        type(varying_string) :: functions_part
        integer :: i
        type(varying_string) :: renamed_functions(size(test_info%function_names))

        if (size(test_info%function_names) == 0) then
            use_statement = ""
        else
            do i = 1, size(test_info%function_names)
                renamed_functions(i) = &
                        "                " &
                        // rename_(test_info%module_name, test_info%function_names(i)) &
                        // " => " // test_info%function_names(i)
            end do
            functions_part = join(renamed_functions, ", &" // NEWLINE)
            use_statement = &
                    "        use " // test_info%module_name // ", only: &" // NEWLINE &
                    // functions_part
        end if
    end function

    pure function rename_(module_name, function_name) result(renamed)
        use iso_varying_string, only: varying_string, operator(//), replace

        type(varying_string), intent(in) :: module_name
        type(varying_string), intent(in) :: function_name
        type(varying_string) :: renamed

        renamed = replace(module_name // "_" // function_name, "test_test_", "")
    end function

    pure subroutine make_test_array(test_infos, test_array)
        use iso_varying_string, only: varying_string, assignment(=), operator(//)
        use strff, only: to_string

        type(test_info_t), intent(in) :: test_infos(:)
        type(varying_string), allocatable, intent(out) :: test_array(:)

        integer :: i, j, next_entry
        integer :: num_individual_tests

        num_individual_tests = sum([(size(test_infos(i)%function_names), i = 1, size(test_infos))])
        allocate(test_array(num_individual_tests + 3))

        test_array(1) = &
                "        type(test_item_t) :: individual_tests(" &
                // to_string(num_individual_tests) // ")"
        test_array(2) = ""
        next_entry = 3
        do i = 1, size(test_infos)
            do j = 1, size(test_infos(i)%function_names)
                test_array(next_entry) = &
                        "        individual_tests(" // to_string(next_entry-2) // ") = " &
                        // rename_(test_infos(i)%module_name, test_infos(i)%function_names(j)) &
                        // "()"
                next_entry = next_entry + 1
            end do
        end do
        test_array(next_entry) = "        tests = test_that(individual_tests)"
    end subroutine
end module
