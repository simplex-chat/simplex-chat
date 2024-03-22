#!/usr/bin/env sh

set -eu

APP_NAME="simplex-chat"
BIN_DIR="$HOME/.local/bin"
BIN_PATH="$BIN_DIR/$APP_NAME"
PLATFORM="$(uname)"

if [ -n "${1:-}" ]; then
  RELEASE="tag/$1"
  DOWNLOAD="download/$1"
  echo "downloading SimpleX Chat $1 ..."
else
  RELEASE=latest
  DOWNLOAD="latest/download"
  echo "downloading the latest version of SimpleX Chat ..."
fi

if [ $PLATFORM = "Darwin" ]; then
  PLATFORM="macos-x86-64"
elif [ $PLATFORM = "Linux" ]; then
  PLATFORM="ubuntu-20_04-x86-64"
else
  echo "Scripted installation on your platform is not supported."
  echo "See compiled binaries in the ${1:-latest} release: https://github.com/$APP_NAME/$APP_NAME/releases/$RELEASE"
  exit 1
fi

# / Prepare to upgrade from v0 to v1

# Determine path of chat binary
if [ -n "$(which $APP_NAME)" ]; then
  binary=$(which $APP_NAME)
elif [ -f "$BIN_PATH" ]; then
  binary=$BIN_PATH
else
  binary=""
fi

# If chat binary not found, check v0 initial migration and offer to abort or continue
if [ -z $binary ]; then
  agent_db="$HOME/.simplex/simplex.agent.db"
  if [ -f "$agent_db" ] && \
    echo "select * from migrations;" | sqlite3 $agent_db | grep -q 20210101_initial
  then
    echo "Warning: found SimpleX Chat database, the current version is not backwards compatible."
    echo "If you continue, the current version will be installed as $APP_NAME with a clean database, the old database will be preserved."
    while true; do
      echo "Please choose to (a)bort or (c)ontinue: " 
      read -r yn < /dev/tty
      case $yn in
          [Aa]* ) exit 1 ;;
          [Cc]* ) break ;;
          * ) echo "Please answer 'a' or 'c'."
      esac
    done
  fi
# If chat binary found, check version and offer to abort or continue, on continue rename chat binary
elif ! $binary -h | grep -q v1; then
  echo "Warning: found a previous version of SimpleX Chat, the current version is not backwards compatible."
  echo "If you continue, it will be renamed to $APP_NAME-v0, and the new version will be installed as $APP_NAME with a clean database."
  while true; do
    echo "Please choose to (a)bort or (c)ontinue: " 
    read -r yn < /dev/tty
    case $yn in
        [Aa]* ) exit 1 ;;
        [Cc]* )
          binary_v0="$binary-v0"
          mv ${binary} ${binary_v0}
          echo "Renamed $binary into $binary_v0"
          break
          ;;
        * ) echo "Please answer 'a' or 'c'."
    esac
  done
fi
# Prepare to upgrade from v0 to v1 /

[ ! -d $BIN_DIR ] && mkdir -p $BIN_DIR

if [ -n "$(command -v curl)" ]; then
  curl -L -o $BIN_PATH "https://github.com/$APP_NAME/$APP_NAME/releases/$DOWNLOAD/$APP_NAME-$PLATFORM"
elif [ -n "$(command -v wget)" ]; then
  wget -O $BIN_PATH "https://github.com/$APP_NAME/$APP_NAME/releases/$DOWNLOAD/$APP_NAME-$PLATFORM"
else
  echo "Cannot download $APP_NAME - please install curl or wget"
  exit 1
fi

chmod +x $BIN_PATH

echo "$APP_NAME installed successfully!"

if [ -z "$(command -v $APP_NAME)" ]; then
  case "$SHELL" in
  *zsh) SHELL_FILE="$HOME/.zshrc" ;;
  *bash) SHELL_FILE="$HOME/.bashrc" ;;
  *fish)
    IS_FISH=1
    SHELL_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/fish/config.fish" ;;
  *)
    echo "Unknown shell - cannot add $APP_NAME folder to PATH"
    echo "Please add $BIN_DIR to PATH variable"
    echo "Or you can run $APP_NAME via full path: $BIN_PATH"
    ;;
esac
  if [ -n "${SHELL_FILE:-}" ]; then
    case "$PATH" in
      *"$BIN_DIR"*) ;;
      *)
        if [ -n "${IS_FISH:-}" ]; then
          echo "set -gx PATH \$PATH:$BIN_DIR" >> $SHELL_FILE
        else
          echo "export PATH=\$PATH:$BIN_DIR" >> $SHELL_FILE
        fi
        ;;
    esac
    echo "Source your $SHELL_FILE or open a new shell and type $APP_NAME to run it"
  fi
else
  echo "Type $APP_NAME in your terminal to run it"
fi
