import base64

import pytest

from support_bot_light.config import Config, ConfigError, Health, load_config

VALID = """
[bot]
display_name = "Support"
db_prefix = "./support_bot_light"
welcome = "Hi! Someone will join shortly."

[roster]
group_name = "Invite roster"
member_role = "admin"
"""

# Minimal 1x1 PNG. The loader never parses it, only encodes the bytes.
PNG_BYTES = (
    b"\x89PNG\r\n\x1a\n"
    b"\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x02\x00\x00\x00\x90wS\xde"
    b"\x00\x00\x00\x0cIDATx\x9cc\xf8\xcf\xc0\x00\x00\x03\x01\x01\x00\xc9\xfe\x92\xef"
    b"\x00\x00\x00\x00IEND\xaeB`\x82"
)


def write(tmp_path, text):
    p = tmp_path / "config.toml"
    p.write_text(text, encoding="utf-8")
    return p


def test_loads_all_fields(tmp_path):
    cfg = load_config(write(tmp_path, VALID))
    assert cfg == Config(
        display_name="Support",
        db_prefix="./support_bot_light",
        welcome="Hi! Someone will join shortly.",
        group_name="Invite roster",
        member_role="admin",
        health=Health(host="127.0.0.1", port=8080),
    )


def test_member_role_defaults_to_owner(tmp_path):
    text = VALID.replace('member_role = "admin"\n', "")
    assert load_config(write(tmp_path, text)).member_role == "owner"


def test_rejects_unknown_member_role(tmp_path):
    text = VALID.replace('"admin"', '"chief"')
    with pytest.raises(ConfigError, match="member_role"):
        load_config(write(tmp_path, text))


def test_rejects_missing_key(tmp_path):
    text = VALID.replace('welcome = "Hi! Someone will join shortly."\n', "")
    with pytest.raises(ConfigError, match="bot.welcome"):
        load_config(write(tmp_path, text))


def test_rejects_missing_bot_section(tmp_path):
    with pytest.raises(ConfigError, match=r"missing \[bot\] section"):
        load_config(write(tmp_path, '[roster]\ngroup_name = "R"\n'))


def test_rejects_missing_roster_section(tmp_path):
    with pytest.raises(ConfigError, match=r"missing \[roster\] section"):
        load_config(write(tmp_path, "[bot]\n"))


def test_rejects_empty_string(tmp_path):
    text = VALID.replace('"Invite roster"', '"  "')
    with pytest.raises(ConfigError, match="roster.group_name"):
        load_config(write(tmp_path, text))


def test_missing_file(tmp_path):
    with pytest.raises(ConfigError, match="not found"):
        load_config(tmp_path / "nope.toml")


def test_invalid_toml(tmp_path):
    with pytest.raises(ConfigError, match="invalid TOML"):
        load_config(write(tmp_path, "[bot"))


def test_image_defaults_to_none(tmp_path):
    assert load_config(write(tmp_path, VALID)).image is None


def test_png_image_encodes_with_prefix_and_roundtrips(tmp_path):
    (tmp_path / "avatar.png").write_bytes(PNG_BYTES)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    image = load_config(write(tmp_path, text)).image
    assert image is not None
    prefix = "data:image/png;base64,"
    assert image.startswith(prefix)
    assert base64.b64decode(image[len(prefix) :]) == PNG_BYTES


@pytest.mark.parametrize("ext", ["jpg", "jpeg"])
def test_jpg_and_jpeg_extensions_encode_as_jpg(tmp_path, ext):
    (tmp_path / f"avatar.{ext}").write_bytes(b"not really a jpeg, just bytes")
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        f'db_prefix = "./support_bot_light"\nimage = "avatar.{ext}"',
    )
    image = load_config(write(tmp_path, text)).image
    assert image is not None
    assert image.startswith("data:image/jpg;base64,")


def test_uppercase_extension_accepted(tmp_path):
    (tmp_path / "avatar.PNG").write_bytes(PNG_BYTES)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.PNG"',
    )
    image = load_config(write(tmp_path, text)).image
    assert image is not None
    assert image.startswith("data:image/png;base64,")


def test_rejects_unsupported_extension(tmp_path):
    (tmp_path / "avatar.gif").write_bytes(b"gif bytes")
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.gif"',
    )
    with pytest.raises(ConfigError, match=r"\.gif"):
        load_config(write(tmp_path, text))


def test_rejects_missing_image_file(tmp_path):
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "missing.png"',
    )
    resolved = tmp_path / "missing.png"
    with pytest.raises(ConfigError, match=r"not found.*missing\.png|missing\.png.*not found"):
        load_config(write(tmp_path, text))
    assert not resolved.exists()


def test_rejects_oversized_image(tmp_path):
    # 12500 caps the whole data URI, prefix included.
    (tmp_path / "avatar.png").write_bytes(b"\x00" * 20000)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    with pytest.raises(ConfigError, match="12500"):
        load_config(write(tmp_path, text))


def test_rejects_empty_image_string(tmp_path):
    text = VALID.replace(
        'db_prefix = "./support_bot_light"', 'db_prefix = "./support_bot_light"\nimage = "  "'
    )
    with pytest.raises(ConfigError, match="bot.image"):
        load_config(write(tmp_path, text))


def test_relative_image_path_resolves_against_config_dir(tmp_path, monkeypatch):
    other_dir = tmp_path / "elsewhere"
    other_dir.mkdir()
    monkeypatch.chdir(other_dir)

    (tmp_path / "avatar.png").write_bytes(PNG_BYTES)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    image = load_config(write(tmp_path, text)).image
    assert image is not None
    assert base64.b64decode(image[len("data:image/png;base64,") :]) == PNG_BYTES


def test_absolute_image_path_works(tmp_path):
    image_path = tmp_path / "avatar.png"
    image_path.write_bytes(PNG_BYTES)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        f'db_prefix = "./support_bot_light"\nimage = "{image_path}"',
    )
    image = load_config(write(tmp_path, text)).image
    assert image is not None
    assert base64.b64decode(image[len("data:image/png;base64,") :]) == PNG_BYTES


def test_rejects_empty_image_file(tmp_path):
    (tmp_path / "avatar.png").write_bytes(b"")
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    with pytest.raises(ConfigError, match="empty"):
        load_config(write(tmp_path, text))


def test_rejects_a_non_regular_image_file(tmp_path):
    import os

    os.mkfifo(tmp_path / "avatar.png")
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    with pytest.raises(ConfigError, match="not a regular file"):
        load_config(write(tmp_path, text))


def test_rejects_an_oversized_image_before_reading_it(tmp_path):
    (tmp_path / "avatar.png").write_bytes(b"A" * 20000)
    text = VALID.replace(
        'db_prefix = "./support_bot_light"',
        'db_prefix = "./support_bot_light"\nimage = "avatar.png"',
    )
    with pytest.raises(ConfigError, match="bytes exceeds"):
        load_config(write(tmp_path, text))


def test_rejects_an_over_long_welcome(tmp_path):
    text = VALID.replace(
        'welcome = "Hi! Someone will join shortly."', 'welcome = "' + "x" * 20000 + '"'
    )
    with pytest.raises(ConfigError, match="too long"):
        load_config(write(tmp_path, text))


def test_health_is_on_without_configuration(tmp_path):
    assert load_config(write(tmp_path, VALID)).health == Health(host="127.0.0.1", port=8080)


def test_health_can_be_switched_off(tmp_path):
    assert load_config(write(tmp_path, VALID + "\n[health]\nenabled = false\n")).health is None


def test_health_port_can_be_set(tmp_path):
    config = load_config(write(tmp_path, VALID + "\n[health]\nport = 9999\n"))
    assert config.health == Health(host="127.0.0.1", port=9999, configured=True)


def test_the_default_port_is_not_treated_as_chosen(tmp_path):
    # A port nobody asked for must not be able to stop the bot from starting.
    assert load_config(write(tmp_path, VALID)).health == Health("127.0.0.1", 8080)
    assert load_config(write(tmp_path, VALID)).health.configured is False


def test_health_host_can_be_set(tmp_path):
    config = load_config(write(tmp_path, VALID + '\n[health]\nhost = "0.0.0.0"\nport = 9000\n'))
    assert config.health == Health(host="0.0.0.0", port=9000, configured=True)


@pytest.mark.parametrize(
    "section",
    [
        '[health]\nenabled = "yes"\n',
        "[health]\nport = 0\n",
        "[health]\nport = 65536\n",
        "[health]\nport = true\n",  # TOML booleans are ints in Python
        '[health]\nport = "8080"\n',
        '[health]\nport = 8080\nhost = " "\n',
    ],
)
def test_invalid_health_settings_are_rejected(tmp_path, section):
    with pytest.raises(ConfigError):
        load_config(write(tmp_path, VALID + "\n" + section))


def test_a_missing_config_points_at_the_template(tmp_path):
    # Under Docker this is a restart loop until the operator acts, so the error
    # has to say what the action is.
    (tmp_path / "config.toml.example").write_text(VALID, encoding="utf-8")
    with pytest.raises(ConfigError, match="copy config.toml.example to config.toml"):
        load_config(tmp_path / "config.toml")


def test_a_missing_config_without_a_template_says_only_that(tmp_path):
    with pytest.raises(ConfigError, match="not found") as raised:
        load_config(tmp_path / "config.toml")
    assert "copy" not in str(raised.value)
