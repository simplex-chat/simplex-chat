#!/usr/bin/env sh
set -eu

TAG="$1"

tempdir="$(mktemp -d)"
init_dir="$PWD"

ghc='9.6.3'

repo_name="simplex-chat"
repo="https://github.com/simplex-chat/${repo_name}"

image_name='sx-local'
container_name='sx-builder'

cabal_local='ignore-project: False
package direct-sqlcipher
    flags: +openssl'

export DOCKER_BUILDKIT=1

version=${TAG#v}
version=${version%-*}

cleanup() {
    docker exec -t "${container_name}" sh -c 'rm -rf ./dist-newstyle ./apps' 2>/dev/null || :
    rm -rf -- "${tempdir}"
    docker rm --force "${container_name}" 2>/dev/null || :
    docker image rm "${image_name}" 2>/dev/null || :
    cd "${init_dir}"
}
trap 'cleanup' EXIT INT

mkdir -p "${init_dir}/${TAG}-${repo_name}/from-source" "${init_dir}/${TAG}-${repo_name}/prebuilt"

git -C "${tempdir}" clone "${repo}.git" &&\
    cd "${tempdir}/${repo_name}" &&\
    git checkout "${TAG}"

oses="22.04@sha256:5c8b2c0a6c745bc177669abfaa716b4bc57d58e2ea3882fb5da67f4d59e3dda5 24.04@sha256:98ff7968124952e719a8a69bb3cccdd217f5fe758108ac4f21ad22e1df44d237"

for os_pair in ${oses}; do
    os="${os_pair%@*}"
    hash="${os_pair#*@}"
    os_url="$(printf '%s' "${os}" | tr '.' '_')"

    cli_name="simplex-chat-ubuntu-${os_url}-x86_64"
    deb_name="simplex-desktop-ubuntu-${os_url}-x86_64.deb"
    appimage_name="simplex-desktop-x86_64.AppImage"

    # Build image
    docker build \
        --no-cache \
        --build-arg TAG="${os}" \
        --build-arg HASH="${hash}" \
        --build-arg GHC="${ghc}" \
        --build-arg=USER_UID="$(id -u)" \
        --build-arg=USER_GID="$(id -g)" \
        -f "${tempdir}/${repo_name}/Dockerfile.build" \
        -t "${image_name}" \
        .

    printf '%s' "${cabal_local}" > "${tempdir}/${repo_name}/cabal.project.local"

    # Run container in background
    docker run -t -d \
        --name "${container_name}" \
        --device /dev/fuse \
        --cap-add SYS_ADMIN \
        --security-opt apparmor:unconfined \
        -v "${tempdir}/${repo_name}:/project" \
        "${image_name}"

    # Consistent permissions
    docker exec \
        -t "${container_name}" \
        sh -c 'find /project -type d -exec chmod 755 {} \; ; find /project -type f -perm /111 -exec chmod 755 {} \; ; find /project -type f ! -perm /111 -exec chmod 644 {} \;'

    # CLI
    docker exec \
        -t "${container_name}" \
        sh -c 'cabal clean && cabal update && cabal build -j && mkdir -p /out && for i in simplex-chat; do bin=$(find /project/dist-newstyle -name "$i" -type f -executable) && chmod +x "$bin" && mv "$bin" /out/; done && strip /out/simplex-chat'

    # Copy CLI
    docker cp \
        "${container_name}":/out/simplex-chat \
        "${init_dir}/${TAG}-${repo_name}/from-source/${cli_name}"

    # Download prebuilt CLI binary
    curl -L \
        --output-dir "${init_dir}/${TAG}-${repo_name}/prebuilt/" \
        -O "${repo}/releases/download/${TAG}/${cli_name}"

    # CLI: deb
    docker exec \
        -t "${container_name}" \
        sh -c "./scripts/desktop/build-cli-deb.sh ${version}"

    # Copy CLI: deb
    docker cp \
        "${container_name}":/out/deb-build/simplex-chat.deb \
        "${init_dir}/${TAG}-${repo_name}/from-source/${cli_name}.deb"

    # Download prebuilt CLI: deb binary
    curl -L \
        --output-dir "${init_dir}/${TAG}-${repo_name}/prebuilt/" \
        -O "${repo}/releases/download/${TAG}/${cli_name}.deb"

    # Desktop: deb
    docker exec \
        -t "${container_name}" \
        sh -c './scripts/desktop/make-deb-linux.sh'

    # Copy deb
    docker cp \
        "${container_name}":/project/apps/multiplatform/release/main/deb/simplex_x86_64.deb \
        "${init_dir}/${TAG}-${repo_name}/from-source/${deb_name}"

    # Download prebuilt deb package
    curl -L \
        --output-dir "${init_dir}/${TAG}-${repo_name}/prebuilt/" \
        -O "${repo}/releases/download/${TAG}/${deb_name}"

    # Desktop: appimage. Build only on 22.04
    case "$os" in
        22.04)
            # Appimage
            docker exec \
                -t "${container_name}" \
                sh -c './scripts/desktop/make-appimage-linux.sh && mv ./apps/multiplatform/release/main/*imple*.AppImage ./apps/multiplatform/release/main/simplex.appimage'

            # Copy appimage
            docker cp \
                "${container_name}":/project/apps/multiplatform/release/main/simplex.appimage \
                "${init_dir}/${TAG}-${repo_name}/from-source/${appimage_name}"

            # Download prebuilt appimage binary
            curl -L \
                --output-dir "${init_dir}/${TAG}-${repo_name}/prebuilt/" \
                -O "${repo}/releases/download/${TAG}/${appimage_name}"
            ;;
    esac

    # Important! Remove dist-newstyle for the next interation
    docker exec \
        -t "${container_name}" \
        sh -c 'rm -rf ./dist-newstyle ./apps/multiplatform'

    # Also restore git to previous state
    git reset --hard && git clean -dfx

    # Stop containers, delete images
    docker stop "${container_name}"
    docker rm --force "${container_name}"
    docker image rm "${image_name}"
done

# Cleanup
rm -rf -- "${tempdir}"
cd "${init_dir}"

# Final stage: compare hashes

# Path to binaries
path_bin="${init_dir}/${TAG}-${repo_name}"

# Assume everything is okay for now
bad=0

# Check hashes for all binaries
for file in "${path_bin}"/from-source/*; do
    # Extract binary name
    app="$(basename ${file})"

    # Compute hash for compiled binary
    compiled=$(sha256sum "${path_bin}/from-source/${app}" | awk '{print $1}')
    # Compute hash for prebuilt binary
    prebuilt=$(sha256sum "${path_bin}/prebuilt/${app}" | awk '{print $1}')

    # Compare
    if [ "${compiled}" != "${prebuilt}" ]; then
        # If hashes doesn't match, set bad...
        bad=1

        # ... and print affected binary
        printf "%s - sha256sum hash doesn't match\n" "${app}"
    fi
done

# If everything is still okay, compute checksums file
if [ "${bad}" = 0 ]; then
    sha256sum "${path_bin}"/from-source/* | sed -e "s|$PWD/||g" -e 's|from-source/||g' -e "s|-$repo_name||g" > "${path_bin}/_sha256sums"

    printf 'Checksums computed - %s\n' "${path_bin}/_sha256sums"
fi
