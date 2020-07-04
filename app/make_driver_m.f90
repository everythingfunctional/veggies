module make_driver_m
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(//), &
            char, &
            extract, &
            get, &
            index, &
            put, &
            replace, &
            var_str
    use parff, only: &
            ParsedCharacter_t, &
            ParsedItems_t, &
            ParsedString_t, &
            ParsedValue_t, &
            ParseResult_t, &
            ParserOutput_t, &
            State_t, &
            ConsumedOk, &
            dropThen, &
            either, &
            EmptyError, &
            many, &
            many1, &
            Message, &
            parseChar, &
            parseDigit, &
            parseString, &
            parseWith, &
            satisfy, &
            sequence, &
            thenDrop, &
            withLabel
    use strff, only: operator(.includes.), join, toString, NEWLINE

    implicit none
    private

    type :: TestInfo_t
        type(VARYING_STRING) :: module_name
        type(VARYING_STRING), allocatable :: function_names(:)
    end type TestInfo_t

    interface operator(.startsWith.)
        module procedure startsWith
    end interface operator(.startsWith.)

    public :: makeDriver
contains
    subroutine makeDriver(driver_file, test_files)
        type(VARYING_STRING), intent(in) :: driver_file
        type(VARYING_STRING), intent(in) :: test_files(:)

        integer :: file_unit
        type(VARYING_STRING) :: program_
        type(TestInfo_t) :: test_infos(size(test_files))

        call getTestInfo(test_files, test_infos)
        program_ = makeProgram(takeFileName(dropExtension(driver_file)), test_infos)
        open(newunit = file_unit, file = char(driver_file), action = "WRITE", status = "REPLACE")
        call put(file_unit, program_)
        close(file_unit)
    end subroutine makeDriver

    subroutine getTestInfo(filenames, test_infos)
        type(VARYING_STRING), intent(in) :: filenames(:)
        type(TestInfo_t), intent(out) :: test_infos(size(filenames))

        integer :: i

        do i = 1, size(filenames)
            call getIndividualTestInfo(filenames(i), test_infos(i))
        end do
    end subroutine getTestInfo

    subroutine getIndividualTestInfo(filename, test_info)
        type(VARYING_STRING), intent(in) :: filename
        type(TestInfo_t), intent(out) :: test_info

        test_info%module_name = takeFileName(dropExtension(filename))
        call scanTestFile(filename, test_info%function_names)
    end subroutine getIndividualTestInfo

    pure function takeFileName(filename) result(just_file)
        type(VARYING_STRING), intent(in) :: filename
        type(VARYING_STRING) :: just_file

        just_file = extract(filename, start = index(filename, "/", back=.true.) + 1)
    end function takeFileName

    pure function dropExtension(filename) result(without_extension)
        type(VARYING_STRING), intent(in) :: filename
        type(VARYING_STRING) :: without_extension

        integer :: slash_position
        integer :: dot_position

        slash_position = index(filename, "/", back=.true.)
        dot_position = index(filename, ".", back=.true.)

        if (dot_position <= slash_position) then
            without_extension = filename
        else
            without_extension = extract(filename, finish = dot_position-1)
        end if
    end function dropExtension

    subroutine scanTestFile(filename, function_names)
        type(VARYING_STRING), intent(in) :: filename
        type(VARYING_STRING), allocatable, intent(out) :: function_names(:)

        logical, allocatable :: function_mask(:)
        type(ParseResult_t), allocatable :: function_name_results(:)
        integer :: i
        type(VARYING_STRING), allocatable :: lines(:)
        type(ParseResult_t), allocatable :: maybe_function_names(:)
        integer :: num_function_names

        call readFileLines(filename, lines)
        allocate(maybe_function_names(size(lines)))
        maybe_function_names = parseLine(lines)
        num_function_names = count(maybe_function_names%ok)
        allocate(function_name_results(num_function_names))
        allocate(function_mask(size(lines)))
        function_mask = maybe_function_names%ok
        function_name_results = pack(maybe_function_names, function_mask)
        allocate(function_names(num_function_names))
        do i = 1, num_function_names
            select type (name => function_name_results(i)%parsed)
            type is (ParsedString_t)
                function_names(i) = name%value_
            end select
        end do
    end subroutine

    subroutine readFileLines(filename, lines)
        type(VARYING_STRING), intent(in) :: filename
        type(VARYING_STRING), allocatable, intent(out) :: lines(:)

        integer :: file_unit
        integer :: i
        integer :: num_lines
        integer :: stat
        type(VARYING_STRING) :: tmp

        open(newunit = file_unit, file = char(filename), action = "READ", status = "OLD")
        num_lines = 0
        do
            call get(file_unit, tmp, NEWLINE, iostat = stat)
            if (stat /= 0) exit
            num_lines = num_lines + 1
        end do
        close(file_unit)

        allocate(lines(num_lines))
        open(newunit = file_unit, file = char(filename), action = "READ", status = "OLD")
        do i = 1, num_lines
            call get(file_unit, lines(i), NEWLINE)
        end do
        close(file_unit)
    end subroutine readFileLines

    elemental function parseLine(line) result(maybe_name)
        type(VARYING_STRING), intent(in) :: line
        type(ParseResult_t) :: maybe_name

        maybe_name = parseWith(parseTestFunctionName, line)
    end function parseLine

    pure function parseTestFunctionName(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = thenDrop( &
                dropThen( &
                        dropThen(parseAtLeastOneWhiteSpace, parseFunction, the_state), &
                        parseValidIdentifier), &
                parseSpaceOrOpenParen)
        if (the_result%ok) then
            select type (the_name => the_result%parsed)
            type is (ParsedString_t)
                if (.not. (the_name%value_.startsWith."test_")) then
                    the_result = EmptyError(Message( &
                            the_state%position, the_name%value_, [var_str("test_*")]))
                end if
            end select
        end if
    end function parseTestFunctionName

    pure function parseFunction(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = parseString("function", the_state)
    end function parseFunction

    pure function parseSpaceOrOpenParen(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = either(parseAtLeastOneWhiteSpace, parseOpenParen, the_state)
    end function parseSpaceOrOpenParen

    pure function parseOpenParen(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = parseChar("(", the_state)
    end function parseOpenParen

    pure function parseAtLeastOneWhiteSpace(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = many1(parseWhiteSpace, the_state)
    end function parseAtLeastOneWhiteSpace

    pure function parseValidIdentifier(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = sequence( &
                parseValidFirstCharacter, &
                thenParseValidIdentifiers, &
                the_state)
    contains
        pure function thenParseValidIdentifiers(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            integer :: i
            type(ParsedString_t) :: parsed_identifier

            select type (previous)
            type is (ParsedCharacter_t)
                parsed_identifier%value_ = previous%value_
                result_ = many(parseValidIdentifierCharacter, state_)
                if (result_%empty) then
                    result_ = ConsumedOk( &
                            parsed_identifier, &
                            state_%input, &
                            state_%position, &
                            Message( &
                                    state_%position, &
                                    var_str(""), &
                                    [VARYING_STRING::]))
                else
                    select type (results => result_%parsed)
                    type is (ParsedItems_t)
                        do i = 1, size(results%items)
                            select type (next_char => results%items(i)%item)
                            type is (ParsedCharacter_t)
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
        end function thenParseValidIdentifiers
    end function parseValidIdentifier

    pure function parseValidFirstCharacter(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = parseAlphabet(the_state)
    end function parseValidFirstCharacter

    pure function parseValidIdentifierCharacter(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = either(parseAlphabet, parseNonLetter, the_state)
    contains
        pure function parseNonLetter(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = either(parseDigit, parseUnderscore, state_)
        end function parseNonLetter
    end function parseValidIdentifierCharacter

    pure function parseAlphabet(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("letter", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        pure function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=*), parameter :: LETTER = "abcdefghijklmnopqrstuvwxyz"

            matches = LETTER.includes.char_
        end function theMatcher
    end function parseAlphabet

    pure function parseUnderscore(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = parseChar("_", the_state)
    end function parseUnderscore

    pure function parseWhiteSpace(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("whitespace", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        pure function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=1), parameter :: TAB = char(9)
            character(len=1), parameter :: SPACE = char(32)
            character(len=*), parameter :: WHITESPACE = TAB // SPACE

            matches = WHITESPACE.includes.char_
        end function theMatcher
    end function parseWhiteSpace

    pure function startsWith(string, substring)
        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: startsWith

        startsWith = index(string, substring) == 1
    end function startsWith

    pure function makeProgram(driver_name, test_infos) result(program_)
        type(VARYING_STRING), intent(in) :: driver_name
        type(TestInfo_t), intent(in) :: test_infos(:)
        type(VARYING_STRING) :: program_

        type(VARYING_STRING), allocatable :: test_array(:)
        type(VARYING_STRING) :: use_statements(size(test_infos))

        call makeTestArray(test_infos, test_array)
        use_statements = makeUseStatements(test_infos)

        program_ = join( &
                ["program " // driver_name, &
                use_statements, &
                var_str("    use Vegetables_m, only: TestItem_t, testThat, runTests"), &
                var_str(""), &
                var_str("    implicit none"), &
                var_str(""), &
                var_str("    call run()"), &
                var_str("contains"), &
                var_str("    subroutine run()"), &
                var_str("        type(TestItem_t) :: tests"), &
                test_array, &
                var_str(""), &
                var_str("        call runTests(tests)"), &
                var_str("    end subroutine run"), &
                var_str("end program")], &
                NEWLINE)
    end function makeProgram

    pure function makeUseStatements(test_infos) result(use_statements)
        type(TestInfo_t), intent(in) :: test_infos(:)
        type(VARYING_STRING) :: use_statements(size(test_infos))

        integer :: i

        do i = 1, size(test_infos)
            use_statements(i) = makeUseStatement(test_infos(i))
        end do
    end function makeUseStatements

    pure function makeUseStatement(test_info) result(use_statement)
        type(TestInfo_t), intent(in) :: test_info
        type(VARYING_STRING) :: use_statement

        type(VARYING_STRING) :: functions_part
        integer :: i
        type(VARYING_STRING) :: renamed_functions(size(test_info%function_names))

        if (size(test_info%function_names) == 0) then
            use_statement = ""
        else
            do i = 1, size(test_info%function_names)
                renamed_functions(i) = &
                        "            " &
                        // rename_(test_info%module_name, test_info%function_names(i)) &
                        // " => " // test_info%function_names(i)
            end do
            functions_part = join(renamed_functions, ", &" // NEWLINE)
            use_statement = &
                    "    use " // test_info%module_name // ", only: &" // NEWLINE &
                    // functions_part
        end if
    end function makeUseStatement

    pure function rename_(module_name, function_name) result(renamed)
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: function_name
        type(VARYING_STRING) :: renamed

        renamed = replace(module_name // "_" // function_name, "test_test_", "")
    end function rename_

    pure subroutine makeTestArray(test_infos, test_array)
        type(TestInfo_t), intent(in) :: test_infos(:)
        type(VARYING_STRING), allocatable, intent(out) :: test_array(:)

        integer :: i, j, next_entry
        integer :: num_individual_tests

        num_individual_tests = sum([(size(test_infos(i)%function_names), i = 1, size(test_infos))])
        allocate(test_array(num_individual_tests + 3))

        test_array(1) = &
                "        type(TestItem_t) :: individual_tests(" &
                // toString(num_individual_tests) // ")"
        test_array(2) = ""
        next_entry = 3
        do i = 1, size(test_infos)
            do j = 1, size(test_infos(i)%function_names)
                test_array(next_entry) = &
                        "        individual_tests(" // toString(next_entry) // ") = " &
                        // rename_(test_infos(i)%module_name, test_infos(i)%function_names(j)) &
                        // "()"
                next_entry = next_entry + 1
            end do
        end do
        test_array(next_entry) = "        tests = testThat(individual_tests)"
    end subroutine makeTestArray
end module make_driver_m
