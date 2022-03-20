#!/usr/bin/env bash

DOWNLOADS_DIR="$HOME/Downloads"
echo
read -rn 1 -p "What is your download dir? (default '$DOWNLOADS_DIR'): " ALT_DOWNLOADS_DIR
DOWNLOADS_DIR="${ALT_DOWNLOADS_DIR:-$DOWNLOADS_DIR}"
DOWNLOAD_PATH="$DOWNLOADS_DIR/canisters"

rm -rf "$DOWNLOAD_PATH"
mkdir -p "$DOWNLOAD_PATH"

curl -SL github.com/papyrs/canisters/archive/main.zip | tar -xf - -C "$DOWNLOAD_PATH"

DEST_DIR="./canisters/canisters"

rm -rf "$DEST_DIR"
mkdir -p "$DEST_DIR"

cp -r "$DOWNLOAD_PATH/canisters-main/src" "$DEST_DIR"

cp ./canisters/src/env.mo "$DEST_DIR/src"

dfx deploy --network=ic --no-wallet
