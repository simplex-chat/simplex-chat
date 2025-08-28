import asyncio
from simpx import BotProfile, SimpleXBot
from simpx.download import SimpleXDaemon
from simpx.extension import ChatWrapper
from collections import defaultdict
import traceback
import os
import time

chat_histories = defaultdict(list)

try:
  from openai import OpenAI
  aiclient = OpenAI(
    base_url="https://ai.runonflux.com/v1",
    api_key=os.getenv("OPENAI_API_KEY"),
  )
except ImportError:
  aiclient = None

CHAT_MODEL = "Llama 3.1 8B"

# Example usage
if __name__ == "__main__":

    daemon = SimpleXDaemon()
    daemon.run(port_number=5225)
    
    # Create a bot profile
    profile = BotProfile(
        display_name="Hello World",
        full_name="SimpleX Bot",
        description="An example bot using SimpX-py framework",
        welcome_message="Hello {name}! I'm an example bot. Try !help to see what I can do.",
        auto_accept_message="This is the example bot!",
        command_prefix="/"
    )
    
    # Create the bot with the profile
    bot = SimpleXBot(profile)
    
    @bot.command(name="info", help="Shows bot information")
    async def info_command(chat_info, profile):
        """Command that shows information about the bot's profile."""
        await bot.send_message(
            chat_info,
            f"*Bot Information*\n"
            f"Name: {profile.display_name}\n"
            f"Description: {profile.description}\n"
            f"Address: {profile.address}"
        )

    @bot.command(name="reset", help="Reset the conversation history for this chat")
    async def reset_command(chat_info):
        chat_type = chat_info.get("type")
        if chat_type == "direct":
            chat_id = chat_info["contact"]["contactId"]
        elif chat_type == "group":
            chat_id = chat_info["groupInfo"]["groupId"]
        else:
            return

        chat_histories[chat_id] = []          # wipe the list
        await bot.send_message(chat_info, "Conversation history has been reset.")
    
    if aiclient is not None:
      @bot.on_message()
      async def handle_plain_message(text: str, chat_info: dict, chat_item: dict):
          """
          Demonstrate live messaging with an AI streaming response from OpenRouter.
          
          Args:
              chat_info: Chat information dictionary.
              args: User's prompt.
          """

          # Create a ChatWrapper instance using the provided chat_info and the bot's client.
          chat = ChatWrapper(chat_info, bot.client)

          chat_type = chat_info.get("type")
          if chat_type == "direct":
            chat_id = chat_info["contact"]["contactId"]
          elif chat_type == "group":
            chat_id = chat_info["groupInfo"]["groupId"]
          else:
            return

          # add the new user message to the history
          chat_histories.setdefault(chat_id, [])
          chat_histories[chat_id].append({"role": "user", "content": text})
          
          try:
              # Start a live message session with an initial text.
              initial_text = "Thinking..."
              live_msg = await bot.send_message(chat, initial_text, live=True, ttl=60)
              current_response = ""
              
              
              # Create the chat completion request with streaming enabled.
              response = aiclient.chat.completions.create(
                  model=CHAT_MODEL,
                  messages=chat_histories[chat_id],
                  stream=True  # Enable streaming of the response.
              )

              # Buffer to accumulate tokens until a sentence is complete
              buffer = ""
              last_update = time.time()
              UPDATE_INTERVAL = 2  # seconds

              async def flush_buffer(force: bool = False):
                  """Push complete sentences to the live message at UPDATE_INTERVAL-second cadence."""
                  nonlocal buffer, current_response, last_update
                  now = time.time()

                  if force or (buffer.rstrip().endswith(('.', '?', '!'))
                              and now - last_update >= UPDATE_INTERVAL):
                      current_response += buffer
                      await live_msg.update_live(current_response.strip())
                      buffer = ""
                      last_update = now

              # Process each streaming chunk.
              for chunk in response:
                  # Extract text content from the current chunk.
                  # Assumes chunk structure similar to OpenAI's streaming response.
                  chunk_text = chunk.choices[0].delta.content or ""
                  if "<|eot_id|>" in chunk_text:
                      chunk_text = chunk_text.replace("<|eot_id|>", "")
                      if chunk_text:                       # keep any leftover text
                        buffer += chunk_text
                      break                               # no more data expected
                  buffer += chunk_text
                  await flush_buffer()

              # Final flush for any remaining text
              await flush_buffer(force=True)
              
              chat_histories[chat_id].append({"role": "assistant", "content": current_response})
              # Finalize the live message.
              await live_msg.finish_live()
          
          except Exception as e:
              # In case of error, finalize the live message.
              traceback.print_exc()
              await live_msg.finish_live()

    # Start the bot
    asyncio.run(bot.start())
