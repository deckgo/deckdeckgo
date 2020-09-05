#!/bin/sh

# https://cloud.google.com/storage/docs/configuring-cors?hl=fr

gsutil cors set cors-studio-beta.json gs://deckdeckgo-studio-beta.appspot.com
