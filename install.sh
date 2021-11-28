APP_NAME="simplex-chat"
TARGET_DIR="$HOME/bin"
PLATFORM="$(uname)"

if [ $PLATFORM == "Darwin" ]; then
	PLATFORM="macos-x86-64"
elif [ $PLATFORM == "Linux" ]; then
	PLATFORM="ubuntu-20_04-x86-64"
else
	echo "Scripted installation on your platform is not supported."
	echo "See compiled binaries in the latest release: https://github.com/simplex-chat/simplex-chat/releases/latest
	exit 1
fi

[ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

wget -O $TARGET_DIR/$APP_NAME "https://github.com/$APP_NAME/$APP_NAME/releases/latest/download/$APP_NAME-$PLATFORM" && chmod +x $TARGET_DIR/$APP_NAME

echo "$APP_NAME was installed sucesfully!"
echo "Type simplex-chat in your terminal to run it"
