#!/bin/bash

set -eu

APP_NAME="simplex-chat"
BIN_DIR="$HOME/.local/bin"
BIN_PATH="$BIN_DIR/$APP_NAME"
PLATFORM="$(uname)"

if [ -n "${1:-}" ]; then
  RELEASE="tag/$1"
  DOWNLOAD="download/$1"
  echo "pobieranie SimpleX Chat $1 ..."
else
  RELEASE=latest
  DOWNLOAD="latest/download"
  echo "pobieranie najnowszej wersji SimpleX Chat ..."
fi

if [ $PLATFORM == "Darwin" ]; then
  PLATFORM="macos-x86-64"
elif [ $PLATFORM == "Linux" ]; then
  PLATFORM="ubuntu-20_04-x86-64"
else
  echo "Instalacja skryptowa na twojej platformie nie jest obsługiwana."
  echo "Zobacz skompilowane binaria w ${1:-latest} wydanie: https://github.com/$APP_NAME/$APP_NAME/releases/$RELEASE"
  exit 1
fi

# / Prepare to upgrade from v0 to v1

# Determine path of chat binary
if [[ -n "$(which $APP_NAME)" ]]; then
  binary=$(which $APP_NAME)
elif [[ -f "$BIN_PATH" ]]; then
  binary=$BIN_PATH
else
  binary=""
fi

# If chat binary not found, check v0 initial migration and offer to abort or continue
if [[ -z $binary ]]; then
  agent_db="$HOME/.simplex/simplex.agent.db"
  if [[ \
    -f "$agent_db" && \
    $(echo "wybierz * z migracji;" | sqlite3 $agent_db | grep 20210101_initial) \
  ]]; then
    echo "Ostrzeżenie: znaleziono bazę SimpleX Chat, obecna wersja nie jest wstecznie kompatybilna."
    echo "Jeśli będziesz kontynuować, aktualna wersja zostanie zainstalowana jako $APP_NAME z czystą bazą danych, stara baza danych zostanie zachowana."
    while true; do
      read -p "Proszę wybrać: (p)rzerwij lub (k)ontynuuj: " yn < /dev/tty
      case $yn in
          [Pp]* ) exit 1 ;;
          [Kk]* ) break ;;
          * ) echo "Proszę odpowiedzieć 'p' lub 'k'."
      esac
    done
  fi
# If chat binary found, check version and offer to abort or continue, on continue rename chat binary
elif [[ ! $($binary -h | grep v1) ]]; then
  echo "Ostrzeżenie: znaleziono poprzednią wersję SimpleX Chat, obecna wersja nie jest wstecznie kompatybilna."
  echo "Jeśli będziesz kontynuować, jej nazwa zostanie zmieniona na $APP_NAME-v0, a nowa wersja zostanie zainstalowana jako $APP_NAME z czystą bazą danych."
  while true; do
    read -p "Proszę wybrać: (p)rzerwij lub (k)ontynuuj: " yn < /dev/tty
    case $yn in
        [Pp]* ) exit 1 ;;
        [Kk]* )
          binary_v0="$binary-v0"
          mv ${binary} ${binary_v0}
          echo "Zmieniono nazwę $binary na $binary_v0"
          break
          ;;
        * ) echo "Proszę odpowiedzieć 'p' lub 'k'."
    esac
  done
fi
# Prepare to upgrade from v0 to v1 /

[[ ! -d $BIN_DIR ]] && mkdir -p $BIN_DIR

if [ -n "$(command -v curl)" ]; then
  curl -L -o $BIN_PATH "https://github.com/$APP_NAME/$APP_NAME/releases/$DOWNLOAD/$APP_NAME-$PLATFORM"
elif [ -n "$(command -v wget)" ]; then
  wget -O $BIN_PATH "https://github.com/$APP_NAME/$APP_NAME/releases/$DOWNLOAD/$APP_NAME-$PLATFORM"
else
  echo "Nie można pobrać $APP_NAME - proszę zainstalować curl lub wget"
  exit 1
fi

chmod +x $BIN_PATH

echo "$APP_NAME zainstalowany pomyślnie!"

if [ -z "$(command -v $APP_NAME)" ]; then
  if [ -n "$($SHELL -c 'echo $ZSH_VERSION')" ]; then
    SHELL_FILE="$HOME/.zshrc"
  elif [ -n "$($SHELL -c 'echo $BASH_VERSION')" ]; then
    SHELL_FILE="$HOME/.bashrc"
  else
    echo "Nieznana powłoka - nie można dodać katalogu $APP_NAME do PATH"
    echo "Proszę dodać $BIN_DIR do zmiennej PATH"
    echo "Możesz też uruchomić $APP_NAME poprzez pełną ścieżkę: $BIN_PATH"
  fi
  if [ -n "$SHELL_FILE" ]; then
    echo "eksport PATH=\$PATH:$BIN_DIR" >> $SHELL_FILE
    echo "Źródło pliku $SHELL_FILE lub otwórz nową powłokę i wpisz $APP_NAME aby ją uruchomić"
  fi
else
  echo "Wpisz $APP_NAME w swoim terminalu, aby go uruchomić"
fi
