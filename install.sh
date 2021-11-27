VERSION="v0.4.2"
APP_NAME="simplex-chat"

######################
PLATAFORM=""

if [ "$(uname)" == "Darwin" ]; then
	PLATAFORM="macos-x86-64"
elif [ "$(uname)" == "Linux" ]; then
	PLATAFORM="ubuntu-20_04-x86-64"
else
	echo "Your platform is not suported, try with macos/linux."
	exit 1
fi

PLATAFORM_BIN="$APP_NAME-$PLATAFORM"
######################

######################
# Detect the shell and the file where to export the path
FILE_TO_EXPORT=""

if [ -n "$($SHELL -c 'echo $ZSH_VERSION')" ]; then
	FILE_TO_EXPORT=".zshrc"
elif [ -n "$($SHELL -c 'echo $BASH_VERSION')" ]; then
	FILE_TO_EXPORT=".bashrc"
else
	echo "Your shell is not suported, try with bash/zsh."
	exit 1
fi
######################

# Check if the directory exists
TARGET_DIR="$HOME/.local/bin"
[ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

# Build the url
URL="https://github.com/$APP_NAME/$APP_NAME/releases/download/$VERSION/$PLATAFORM_BIN"

# Download the binary and make it executable
wget -O $TARGET_DIR/simplex-chat $URL && chmod +x $TARGET_DIR/simplex-chat

# Export the path to the binary
echo "export PATH=\$PATH:$TARGET_DIR" >>$HOME/$FILE_TO_EXPORT

# Exit message with instructions
echo "simplex-chat is installed, source your $FILE_TO_EXPORT or open a new shell"
