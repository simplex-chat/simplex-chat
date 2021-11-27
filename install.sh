VERSION="v0.4.2"
APP_NAME="simplex-chat"
TARGET_DIR="$HOME/.local/bin"

PLATFORM="$(uname)"

if [ $PLATFORM == "Darwin" ]; then
	PLATFORM="macos-x86-64"
elif [ $PLATFORM == "Linux" ]; then
	PLATFORM="ubuntu-20_04-x86-64"
else
	echo "Your platform is not suported, try with macos/linux."
	exit 1
fi

[ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

wget -O $TARGET_DIR/simplex-chat "https://github.com/$APP_NAME/$APP_NAME/releases/download/$VERSION/$PLATFORM" && chmod +x $TARGET_DIR/simplex-chat

echo "export PATH=\$PATH:$TARGET_DIR" >>$HOME/.profile

echo "simplex-chat is installed, source your .profile or open a new shell"
