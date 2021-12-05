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

[ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

if [ -n "$(command -v curl)" ]; then
	curl -o $TARGET_DIR/$APP_NAME "https://github.com/$APP_NAME/$APP_NAME/releases/latest/download/$APP_NAME-$PLATFORM"
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
