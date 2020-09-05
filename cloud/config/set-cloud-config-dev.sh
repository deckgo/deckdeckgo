#!/bin/sh
firebase functions:config:set mailchimp.skip="true" info.mail.skip="true" github.skip="true" deckdeckgo.presentation.url="https://beta.deckdeckgo.io" deckdeckgo.api.skip="true"

firebase functions:config:get
