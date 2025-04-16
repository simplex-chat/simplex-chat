#!/usr/bin/env sh
set -eu

TAG="$1"

tempdir="$(mktemp -d)"
init_dir="$PWD"

repo_name="simplex-chat"
repo="https://github.com/simplex-chat/${repo_name}"

cabal_local='ignore-project: False
package direct-sqlcipher
    flags: +openssl'

export DOCKER_BUILDKIT=1

cleanup() {
	docker exec -t builder sh -c 'rm -rf ./dist-newstyle' 2>/dev/null || :
	rm -rf -- "$tempdir"
	docker rm --force builder 2>/dev/null || :
	docker image rm local 2>/dev/null || :
	cd "$init_dir"
}
trap 'cleanup' EXIT INT

mkdir -p "$init_dir/$TAG-$repo_name/from-source" "$init_dir/$TAG-$repo_name/prebuilt"

git -C "$tempdir" clone "$repo.git" &&\
	cd "$tempdir/${repo_name}" &&\
	git checkout "$TAG"

for os in 22.04 24.04; do
	os_url="$(printf '%s' "$os" | tr '.' '_')"

	# Build image
	docker build \
		--no-cache \
		--build-arg TAG=${os} \
		--build-arg GHC=9.6.3 \
		-f "$tempdir/${repo_name}/Dockerfile.build" \
		-t local \
		.

	printf '%s' "$cabal_local" > "$tempdir/${repo_name}/cabal.project.local"

	# Run container in background
	docker run -t -d \
		--name builder \
		-v "$tempdir/${repo_name}:/project" \
		local

	docker exec \
		-t \
		builder \
		sh -c 'cabal clean && cabal update && cabal build -j --enable-tests && mkdir -p /out && for i in simplex-chat; do bin=$(find /project/dist-newstyle -name "$i" -type f -executable) && chmod +x "$bin" && mv "$bin" /out/; done && strip /out/simplex-chat'

	docker cp \
		builder:/out/simplex-chat \
		"$init_dir/$TAG-$repo_name/from-source/simplex-chat-ubuntu-${os_url}-x86-64"

	# Download prebuilt postgresql binary
	curl -L \
		--output-dir "$init_dir/$TAG-$repo_name/prebuilt/" \
		-O \
		"$repo/releases/download/${TAG}/simplex-chat-ubuntu-${os_url}-x86-64"
	
	# Important! Remove dist-newstyle for the next interation
	docker exec \
		-t \
		builder \
		sh -c 'rm -rf ./dist-newstyle'

	# Also restore git to previous state 
	git reset --hard && git clean -dfx

	# Stop containers, delete images
	docker stop builder
	docker rm --force builder
	docker image rm local
done

# Cleanup
rm -rf -- "$tempdir"
cd "$init_dir"

# Final stage: compare hashes

# Path to binaries
path_bin="$init_dir/$TAG-$repo_name"

# Assume everything is okay for now
bad=0

# Check hashes for all binaries
for file in "$path_bin"/from-source/*; do
	# Extract binary name
	app="$(basename $file)"

	# Compute hash for compiled binary
	compiled=$(sha256sum "$path_bin/from-source/$app" | awk '{print $1}')
	# Compute hash for prebuilt binary
	prebuilt=$(sha256sum "$path_bin/prebuilt/$app" | awk '{print $1}')

	# Compare
	if [ "$compiled" != "$prebuilt" ]; then
		# If hashes doesn't match, set bad...
		bad=1

		# ... and print affected binary
		printf "%s - sha256sum hash doesn't match\n" "$app"
	fi
done

# If everything is still okay, compute checksums file
if [ "$bad" = 0 ]; then
	sha256sum "$path_bin"/from-source/* | sed -e "s|$PWD/||g" -e 's|from-source/||g' -e "s|-$repo_name||g" > "$path_bin/_sha256sums"

	printf 'Checksums computed - %s\n' "$path_bin/_sha256sums"
fi
