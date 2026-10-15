.DEFAULT_GOAL := help

NIX_TARGET ?= all
NIX_ARGS ?=
export NIX_SYSTEM
export NIX_OUT_DIR
export NIX_BUILD_TIMEOUT

.PHONY: help nix-list nix-build nix-native nix-android nix-windows nix-linux-static nix-ios nix-darwin-libs

help: ## Show build commands.
	@awk 'BEGIN { FS = ":.*## " } /^[a-zA-Z_-]+:.*## / { printf "  %-20s %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

# The flake lock selects the compiler; these targets never install a stock GHC.
nix-list: ## List selected Hydra outputs without building (NIX_TARGET=all).
	@./scripts/nix/build.sh --list $(NIX_TARGET)

nix-build: ## Build the host's existing Hydra outputs (NIX_TARGET=all, NIX_ARGS=...).
	./scripts/nix/build.sh $(NIX_TARGET) $(NIX_ARGS)

nix-native: ## Build the native Haskell library and CLI.
	./scripts/nix/build.sh native $(NIX_ARGS)

nix-android: ## Build both Android ABIs and their support libraries on x86_64-linux.
	./scripts/nix/build.sh android $(NIX_ARGS)

nix-windows: ## Cross-build the Windows library and CLI on x86_64-linux.
	./scripts/nix/build.sh windows $(NIX_ARGS)

nix-linux-static: ## Cross-build the x86_64, i686, and aarch64 Linux static CLIs.
	./scripts/nix/build.sh linux-static $(NIX_ARGS)

nix-ios: ## Build the iOS Haskell library ZIP on the selected Darwin host.
	./scripts/nix/build.sh ios $(NIX_ARGS)

nix-darwin-libs: ## Build Swift JSON and tagged JSON library ZIPs on Darwin.
	./scripts/nix/build.sh darwin-libs $(NIX_ARGS)
