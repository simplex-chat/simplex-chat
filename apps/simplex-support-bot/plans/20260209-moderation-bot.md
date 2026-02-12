A SimpleX Chat bot that monitors public groups, summarizes conversations using
   Grok LLM, moderates content, and forwards important messages to a private
  staff group.

  Core Features

  1. Message Summarization
  - Periodically summarizes public group messages using Grok API
  - Posts summaries to the group on a configurable schedule (e.g. daily/hourly)
  - Summaries capture key topics, decisions, and action items

  2. Moderation
  - Detects spam, abuse, and policy violations using Grok
  - Configurable actions per severity: flag-only, auto-delete, or remove member
  - All moderation events are forwarded to the staff group for visibility

  3. Important Message Forwarding
  - Grok classifies messages by importance (urgency, issues, support requests)
  - Forwards important messages to a designated private staff group
  - Includes context: sender, group, timestamp, and reason for flagging

  Configuration

  - GROK_API_KEY — Grok API credentials
  - PUBLIC_GROUPS — list of monitored public groups
  - STAFF_GROUP — private group for forwarded alerts
  - SUMMARY_INTERVAL — how often summaries are generated
  - MODERATION_RULES — content policy and action thresholds

  Non-Goals

  - No interactive Q&A or general chatbot behavior in groups
  - No direct user communication from the bot (all escalation goes to staff
  group)
