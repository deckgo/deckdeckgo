# 7.10.0 (2022-04-21)

### Features

- add optional `attributes` param to `cleanNode`

# 7.9.1 (2022-04-14)

### Features

- clean some more grammarly attributes

# 7.9.0 (2022-04-02)

### Features

- add `config` to `DocPublish`
- add `theme` to `docPublishData`

# 7.8.1 (2022-03-19)

### Fix

- clean code attributes selector

# 7.8.0 (2022-03-19)

### Features

- `cleanNode` extended to clean HTML element children too
- expose list of `dirtyAttributes`

# 7.7.0 (2022-03-19)

### Features

- add `fallbackAuthor` to publish data

# 7.6.1 (2022-03-15)

### Fix

- `docSelector` match only parent `deckgo-studio-doc`

# 7.6.0 (2022-03-15)

### Features

- remove `deckgo-doc` from `docSelector` and replace with css based selector

# 7.5.0 (2022-03-04)

### Features

- image utils to handle deckgo-lazy-img attributes

# 7.4.2 (2022-03-03)

### Fix

- `docSelector` match parent `deckgo-studio-doc`

# 7.4.1 (2022-03-03)

### Features

- `onNext` callback of snapshot to promise

# 7.4.0 (2022-02-27)

### Features

- throw custom events "log" to display events that can be also interesting for the users

# 7.3.0 - 7.3.1 (2022-02-24)

### Features

- throw custom error function on the document

# 7.2.0 - 7.2.1 (2022-02-24)

### Features

- add function `attributes`

# 7.1.0 (2022-02-21)

### Features

- update `docSelector` path

# 7.0.0 (2022-02-20)

### Breaking Changes

- esm and cjs

# 6.0.0 (2021-12-21)

### Breaking Changes

- `deckPublishData` and `docPublishData` become async

### Features

- generate social image for publish data

# 5.1.1 (2021-12-17)

### Fix

- `docPublishData` empty paragraphs

# 5.1.0 (2021-12-17)

### Features

- add `deploy` information to `doc`

# 5.0.0 (2021-12-17)

### Breaking Changes

- `publishData` becomes `deckPublishData` and `docPublishData`

### Features

- add optional `canonical` url to `meta`
- support for `canonical` in `publishData` function

# 4.0.0 (2021-12-16)

### Breaking Changes

- `publish` becomes `deckPublish` and `docPublish`

### Features

- optional `meta` added to `doc`

# 3.3.0 (2021-12-16)

### Features

- `convertStyle` moved from `deck-utils`

# 3.2.0 (2021-12-03)

### Features

- `nodexIndex` returns node index (including comments and texts) and `elementIndex` returns element index

# 3.1.1 (2021-12-03)

### Features

- add `init` to sync state type

# 3.1.0 (2021-11-28)

### Features

- optional `bio` in publish data and deck author

# 3.0.1 (2021-11-28)

### Features

- optional `photo_url` in publish data

# 3.0.0 (2021-11-27)

### Breaking Changes

- rework authentication and delete user

### Features

- function that collect and prepare the (deck) publish data
- new entry to retrieve published urls

# 2.1.0 (2021-11-07)

### Features

- expose providers interfaces for documents and paragraphs

# 2.0.0 (2021-11-05)

### Features

- support for documents and paragraphs

# 1.0.0 (2021-10-21)

### Features

- Hello World 👋
