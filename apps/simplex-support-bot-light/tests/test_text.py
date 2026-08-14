from support_bot_light.text import MAX_NAME, UNNAMED, safe_name


def test_collapses_newlines_so_a_name_cannot_forge_a_line():
    assert safe_name("AAA\n  • ceo@example.com — since 2020-01-01") == (
        "AAA • ceo@example.com — since 2020-01-01"
    )
    assert "\n" not in safe_name("a\r\nb\tc")


def test_truncates_a_long_name():
    out = safe_name("X" * 14000)
    assert len(out) == MAX_NAME
    assert out.endswith("…")


def test_strips_non_printable_characters():
    assert safe_name("bob\x00\x07") == "bob"


def test_blank_and_whitespace_only_names():
    assert safe_name("") == UNNAMED
    assert safe_name("   \n ") == UNNAMED


def test_leaves_an_ordinary_name_alone():
    assert safe_name("Narasimha") == "Narasimha"


def test_strips_invisible_but_printable_characters():
    # Hangul fillers and Braille blanks are Lo/So, so isprintable() lets them
    # through while they render as nothing.
    assert safe_name("\u3164\u3164Alice") == "Alice"
    assert safe_name("\u115f\u1160Alice") == "Alice"
    assert safe_name("\u2800Alice") == "Alice"
    assert safe_name("\u3164" * 10) == UNNAMED


def test_normalises_compatibility_forms():
    assert safe_name("\uff21lice") == "Alice"


def test_leaves_names_in_other_scripts_alone():
    for name in (
        "\uae40\ucca0\uc218",
        "Nguy\u1ec5n",
        "\u0645\u062d\u0645\u062f",
        "Jos\u00e9 M\u00fcller",
    ):
        assert safe_name(name) == name
