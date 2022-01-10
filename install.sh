set -eu

APP_NAME="simplex-chat"
TARGET_DIR="$HOME/.local/bin"
PLATFORM="$(uname)"

if [ $PLATFORM == "Darwin" ]; then
	PLATFORM="macos-x86-64"
elif [ $PLATFORM == "Linux" ]; then
	PLATFORM="ubuntu-20_04-x86-64"
else
	echo "Scripted installation on your platform is not supported."
	echo "See compiled binaries in the latest release: https://github.com/simplex-chat/simplex-chat/releases/latest"
	exit 1
fi

#check binary exists

if which simplex-chat then
  chat_path = which simplex-chat
elif target_dir/simplex-chat exists then
  chat_path = target_dir/simplex-chat
else
  chat_path = nothing
fi

# prepare for upgrade

if chat_path is not nothing then
  if not (simplex-chat -h | grep v1) then
    warn
    ask a/c
      on abort: exit 1
      on continue: move chat_path to chat_path_v0
  elif initial2021 migration exists then
    warn2
    ask a/c
      on abort: exit 1
      on continue: break
  fi
fi

# Prepare to upgrade from v0 to v1
if [[ \
  ! $(simplex-chat -h | grep v1) || \
  $(echo "select * from migrations;" | sqlite3 ~/.simplex/simplex.agent.db | grep 20210101_initial) \
]]; then
  echo "Found previous version of SimpleX Chat, current version v1.0.0 is not compatible."
  echo "If you choose to continue the installation it will be renamed to simplex-chat-v0 and version v1 will be installed as simplex-chat with clean database."
  echo "The next version v1.1.0 will be backwards compatible with your groups and contacts. Please see <link> for more information."
  while true; do
    read -p "Please choose to (a)bort or (c)ontinue: " yn
    case $yn in
        [Aa]* )
          exit 1
          ;;
        [Cc]* )
          chat_path=$(which simplex-chat)
          # if [[ -z "$chat_path" ]]; then
          #   # check if old file exists and write to chat_path
          # fi
          new_chat_path="$chat_path-v0"
          mv ${chat_path} ${new_chat_path}
          echo "Renamed $chat_path into $new_chat_path"
          break
          ;;
        * ) echo "Please answer a or c."
    esac
  done
fi

[ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

if [ -n "$(command -v curl)" ]; then
	curl -L -o $TARGET_DIR/$APP_NAME "https://github.com/$APP_NAME/$APP_NAME/releases/latest/download/$APP_NAME-$PLATFORM"
elif [ -n "$(command -v wget)" ]; then
	wget -O $TARGET_DIR/$APP_NAME "https://github.com/$APP_NAME/$APP_NAME/releases/latest/download/$APP_NAME-$PLATFORM"
else
	echo "Cannot download simplex-chat - please install curl or wget"
	exit 1
fi

chmod +x $TARGET_DIR/$APP_NAME

echo "$APP_NAME installed sucesfully!"

if [ -z "$(command -v simplex-chat)" ]; then
	if [ -n "$($SHELL -c 'echo $ZSH_VERSION')" ]; then
		SHELL_FILE="$HOME/.zshrc"
	elif [ -n "$($SHELL -c 'echo $BASH_VERSION')" ]; then
		SHELL_FILE="$HOME/.bashrc"
	else
		echo "Unknown shell - cannot add simplex-chat folder to PATH"
		echo "Please add $TARGET_DIR to PATH variable"
		echo "Or you can run simplex-chat via full path: $TARGET_DIR/simplex-chat"
	fi
	if [ -n "$SHELL_FILE" ]; then
		echo "export PATH=\$PATH:$TARGET_DIR" >> $SHELL_FILE
		echo "Source your $SHELL_FILE or open a new shell and type simplex-chat to run it"
	fi
else
	echo "Type simplex-chat in your terminal to run it"
fi
