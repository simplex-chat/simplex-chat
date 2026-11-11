# SimpleX: key facts

Facts about SimpleX network and SimpleX Chat, each with the date it was stated and its source. Updated {{ llms_facts.updated }}. Machine-readable version: [simplex.chat/llms/facts.json](https://simplex.chat/llms/facts.json).
{% for group in llms_facts.groups %}
## {{ group.title | safe }}

| Fact | Value | As of | Source |
|---|---|---|---|
{% for item in group.facts -%}
| {{ item.fact | safe }} | {{ item.value | safe }} | {{ item.asOf | safe }} | {% for source in item.sources %}[{{ source.title | safe }}]({{ source.url }}){% if not loop.last %}, {% endif %}{% endfor %} |
{% endfor -%}
{% endfor %}
