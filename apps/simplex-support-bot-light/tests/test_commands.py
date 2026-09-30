from simplex_chat import BotCommand

from support_bot_light.commands import COMMANDS, to_wire


def test_declares_four_commands():
    assert tuple(c.keyword for c in COMMANDS) == ("dm", "list", "leave", "help")


def test_no_command_takes_params():
    # Zero-argument commands send on tap instead of pasting a placeholder.
    assert all(c.params is None for c in COMMANDS)


def test_to_wire_omits_params_when_none():
    wire = to_wire([BotCommand(keyword="list", label="Who gets invited")])
    assert wire == [{"type": "command", "keyword": "list", "label": "Who gets invited"}]
    assert "params" not in wire[0]


def test_to_wire_includes_params_when_set():
    wire = to_wire([BotCommand(keyword="x", label="X", params="<n>")])
    assert wire == [{"type": "command", "keyword": "x", "label": "X", "params": "<n>"}]


def test_to_wire_distinguishes_none_from_empty_string():
    assert "params" not in to_wire([BotCommand("a", "A")])[0]
    assert to_wire([BotCommand("b", "B", params="")])[0]["params"] == ""


def test_to_wire_preserves_declaration_order():
    assert [c["keyword"] for c in to_wire(COMMANDS)] == [c.keyword for c in COMMANDS]
