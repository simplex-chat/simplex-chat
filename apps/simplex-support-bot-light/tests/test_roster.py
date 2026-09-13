from support_bot_light import roster
from tests.conftest import USER_ID, make_contact


def test_entry_of_reads_active_mark():
    contact = make_contact(
        7, "sh", {"supportBotLight": {"roster": "active", "since": "2026-08-13"}}
    )
    entry = roster.entry_of(contact)
    assert entry is not None
    assert (entry.contact_id, entry.name, entry.state, entry.since) == (
        7,
        "sh",
        "active",
        "2026-08-13",
    )


def test_entry_of_returns_none_without_custom_data():
    assert roster.entry_of(make_contact(7, "sh")) is None


def test_entry_of_ignores_other_namespaces():
    assert roster.entry_of(make_contact(7, "sh", {"otherBot": {"roster": "active"}})) is None


def test_entry_of_ignores_unknown_state():
    contact = make_contact(7, "sh", {"supportBotLight": {"roster": "banned"}})
    assert roster.entry_of(contact) is None


def test_entry_of_ignores_non_dict_mark():
    assert roster.entry_of(make_contact(7, "sh", {"supportBotLight": "oops"})) is None


async def test_mark_preserves_other_keys(api):
    contact = make_contact(7, "sh", {"otherBot": {"keep": 1}})
    api.contacts.append(contact)
    await roster.mark(api, contact, "active", "2026-08-13T09:00:00+00:00")
    contact_id, data = api.custom_data[-1]
    assert contact_id == 7
    assert data["otherBot"] == {"keep": 1}
    assert data["supportBotLight"] == {"roster": "active", "since": "2026-08-13T09:00:00+00:00"}


async def test_mark_does_not_mutate_the_callers_contact(api):
    original = {"otherBot": {"keep": 1}}
    contact = make_contact(7, "sh", original)
    api.contacts.append(contact)
    await roster.mark(api, contact, "active", "2026-08-13T09:00:00+00:00")
    # mark() builds a new blob; the caller's dict must be untouched.
    assert original == {"otherBot": {"keep": 1}}
    assert "supportBotLight" not in original


async def test_unmark_removes_only_our_key(api):
    contact = make_contact(
        7, "sh", {"supportBotLight": {"roster": "active"}, "otherBot": {"keep": 1}}
    )
    api.contacts.append(contact)
    await roster.unmark(api, contact)
    assert api.custom_data[-1] == (7, {"otherBot": {"keep": 1}})


async def test_unmark_clears_blob_when_nothing_left(api):
    contact = make_contact(7, "sh", {"supportBotLight": {"roster": "active"}})
    api.contacts.append(contact)
    await roster.unmark(api, contact)
    # None clears the column rather than writing an empty object.
    assert api.custom_data[-1] == (7, None)


async def test_unmark_of_an_unmarked_contact_takes_nothing_away(api):
    contact = make_contact(7, "sh", {"otherBot": {"keep": 1}})
    api.contacts.append(contact)
    await roster.unmark(api, contact)
    assert api.custom_data[-1] == (7, {"otherBot": {"keep": 1}})


async def test_load_returns_marked_contacts_sorted_case_insensitively(api):
    api.contacts += [
        make_contact(1, "Zoe", {"supportBotLight": {"roster": "active", "since": "x"}}),
        make_contact(2, "bob", {"supportBotLight": {"roster": "pending", "since": "x"}}),
        make_contact(3, "unmarked"),
    ]
    entries = await roster.load(api, USER_ID)
    assert [e.name for e in entries] == ["bob", "Zoe"]


async def test_active_filters_pending(api):
    api.contacts += [
        make_contact(
            1, "a", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(2, "b", {"supportBotLight": {"roster": "pending", "since": "x"}}),
    ]
    assert [e.contact_id for e in await roster.active(api, USER_ID)] == [1]


async def test_find_contact_returns_none_when_absent(api):
    assert await roster.find_contact(api, USER_ID, 99) is None


async def test_find_contact_returns_match(api):
    api.contacts.append(make_contact(7, "sh"))
    found = await roster.find_contact(api, USER_ID, 7)
    assert found is not None and found["contactId"] == 7


def test_utc_now_is_iso_with_offset():
    now = roster.utc_now()
    assert now.endswith("+00:00") and "T" in now


async def test_active_excludes_a_marked_contact_that_is_no_longer_usable(api):
    # The person deleted the bot: the mark survives but api_add_member would
    # fail for them on every business chat.
    api.contacts += [
        make_contact(
            1, "live", {"supportBotLight": {"roster": "active", "since": "x"}}, connected=True
        ),
        make_contact(2, "dead", {"supportBotLight": {"roster": "active", "since": "x"}}),
    ]
    assert [e.contact_id for e in await roster.active(api, USER_ID)] == [1]


def test_entry_name_is_sanitised():
    contact = make_contact(7, "a\nb", {"supportBotLight": {"roster": "active", "since": "x"}})
    entry = roster.entry_of(contact)
    assert entry is not None
    assert entry.name == "a b"


def test_entry_of_survives_a_contact_with_no_profile():
    entry = roster.entry_of(
        {"contactId": 7, "customData": {"supportBotLight": {"roster": "active", "since": "x"}}}
    )
    assert entry is not None
    assert entry.name == "(unnamed)"


def test_contact_name_prefers_the_local_display_name():
    # The core makes localDisplayName unique per user; two peers calling
    # themselves "sh" render as "sh" and "sh_1", which is what the roster and
    # the log must show.
    contact = make_contact(1, "sh_1")
    contact["profile"]["displayName"] = "sh"
    assert roster.contact_name(contact) == "sh_1"


def test_contact_name_falls_back_to_the_profile():
    contact = make_contact(1, "sh")
    del contact["localDisplayName"]
    assert roster.contact_name(contact) == "sh"
