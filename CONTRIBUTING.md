# Getting Started Contributing to DeckDeckGo

Hey there 👋

Are you interested to contribute to our open source project? That would be awesome, we are always looking for contributors 👍

The following guide is here to try to guide you through the process of contributing to our project in case you do not know where to get started. If you have further questions, we are happy to help, just ping us on our [Slack](https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY) channel.

## Table of contents

- [How to get Involved](#how-to-get-involved)
- [Technical facts](#technical-facts)
- [Issues](#issues)
- [Bugs and features requests](#bugs-and-features-requests)
- [Translation](#translation)

## How to get Involved

Everyone is welcomed to contribute to our projects, no matter how experienced you are. [DeckDeckGo] was an important learning experience for ourselves and, we would be happy if contributing to our project is also profitable to you in terms of learning.

## Technical facts

[DeckDeckGo] is a mix of applications, components, functions and infrastructure. You could have a look to our [README](README.md) which lists all of these.

In short summary, all our applications and components are developed with [Stencil.JS](https://stenciljs.com) Web Components.

Our website is implemented with [Gatsby](https://www.gatsbyjs.com/).

On the other side, our publication engine is developed with [Haskell](https://www.haskell.org/), [Nix](https://nixos.org/nix/) and [Terraform](https://www.terraform.io/).

## Issues

We are flagging our [issues](https://github.com/deckgo/deckdeckgo/issues) with the labels `good first issue` when we think that the issue is accessible without too much experience and know-how of our project. We are also flagging issue with the label `question` when we need your input. It could be general questions such as brainstorming regarding UX or anything else.

You are welcomed to find these kinds of issues to get started but obviously if you don't find any issue, don't hesitate to take over other types of issues if they inspire you.

In any case, ping us to get start, your help would be super appreciated!

## Bugs and features requests

Some bugs (sh\*t happens) might not yet been reported. Likewise, your awesome ideas to make [DeckDeckGo] better are always wanted. Therefore don't hesitate to open new [issues](https://github.com/deckgo/deckdeckgo/issues) but if you are unsure about if it's the right place or if you would like to discuss it first, go to [Slack](https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY) for that purpose.

## Translation

We would be grateful to get your help to translate our apps!

### Editor

The online editor ([studio](https://github.com/deckgo/deckdeckgo/tree/master/studio)) can be translated as following:

1. Copy [en.json](https://github.com/deckgo/deckdeckgo/blob/master/studio/src/assets/i18n/en.json) to a new filename reflecting the language (such as for example `fr.json` for French)
2. Translate each keys of the newly created file
3. Provide a PR

If you would like to test your translations, either test these by overwriting temporarily [en.json](https://github.com/deckgo/deckdeckgo/blob/master/studio/src/assets/i18n/en.json) or add the new language as following:

1. Add it the to the list of supported languages in [languages.d.ts](https://github.com/deckgo/deckdeckgo/blob/main/studio/src/app/definitions/languages.d.ts)
2. Add a related dynamic import in the state management [i18n.store.ts](https://github.com/deckgo/deckdeckgo/blob/bdc79b9cd1f52171bab2f772fa6905c710f3b2e0/studio/src/app/stores/i18n.store.ts#L11)
3. Add it to the default language detection list in [lang.service.ts](https://github.com/deckgo/deckdeckgo/blob/bdc79b9cd1f52171bab2f772fa6905c710f3b2e0/studio/src/app/services/lang/lang.service.ts#L36)

Translations are handled in JSON files but, as we are consuming these through a store, their representation have to exist as interfaces.
To ease the process we have developed a script which extract the declarations automatically.
In case you would add new keys, run `npm run i18n` to generate the interfaces.

### Site

The new website of [DeckDeckGo] supports i18n. To provide a new language with a Pull Request, follow these steps:

1. Copy each [pages](https://github.com/deckgo/deckdeckgo/tree/master/site/src/pages/) to their new languages. For example `index.en.js` to `index.fr.js` to create a French page.
2. Translate the text of the pages "index, enterprise and discover" in their related [json](https://github.com/deckgo/deckdeckgo/tree/master/site/src/assets/i18n/) data.
3. Translate all other pages directly in their related Javascript files (we did not extract the text from the "simple" page).
4. Provide a translation for the [meta](https://github.com/deckgo/deckdeckgo/blob/master/site/gatsby-config.js) description.

### Remote

The remote control is not yet translated. If you think that's a must, let us now!

### Docs

We don't plan to translate the documentation for developers, it seems like quite a big task. That being said, we are welcoming contributions. If you are up to, for sure that would be awesome!

[deckdeckgo]: https://deckdeckgo.com
