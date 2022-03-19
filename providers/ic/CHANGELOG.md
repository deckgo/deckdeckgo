# 6.2.1 (2022-03-19)

### Features

- sign in css and display button on error

# 6.2.0 (2022-03-18)

### Features

- new timer that checks if the identity delegation is valid

# 6.1.2 (2022-03-15)

### Fix

- `docSelector` match only parent `deckgo-studio-doc`

# 6.1.1 (2022-03-15)

### Fix

- use native `article` HTML element instead of `deckgo-doc` as container for publish

# 6.1.0 (2022-03-15)

### Features

- `docSelector` match parent `deckgo-studio-doc > article.deckgo-doc`

# 6.0.1 (2022-03-14)

### Features

- test delegation valid before sync

### Fix

- env `kitPath` is set only after init

# 6.0.0 (2022-03-13)

### Features

- runtime variables replace static configuration:

a. manager and (optional) identity canister IDs
b. kit path

# 5.3.5 (2022-03-04)

### Fix

- prevent transforming chunk to arrayBuffer error

# (5.3.3) 5.3.4 (2022-03-03)

### Fix

- `docSelector` match parent `deckgo-studio-doc`

# 5.3.2 (2022-03-03)

### Fix

- snapshot published data not original data

# 5.3.1 (2022-03-03)

### Features

- `onNext` of snapshots as promise

# 5.3.0 (2022-03-02)

### Features

- use `ic0.app` as `host` for the agent in production (regardless if connection is on the window or web worker side)

# 5.2.0 (2022-02-27)

### Features

- emit activity

# 5.1.0 (2022-02-24)

### Features

- rename sign-in events for new login
- replace `uuid` with `nanoid`

# 5.1.0 (2022-02-22)

### Features

- update `docSelector` for new `studio` component
- remove ionic peer dependency
- update sign-in component for Papyrs

# 4.1.3 (2021-12-24)

### Features

- add an `<article/>` around the published doc content
- emit doc and deck published once the overview has been updated too

# 4.1.2 (2021-12-22)

### Build

- upgrade doc css resources

# 4.1.1 (2021-12-21)

### Fix

- storage `fullPath` contains begin slash (avoid path like "...ic0.app//images/...")

# 4.1.0 (2021-12-21)

### Features

- publish social images
- meta pathname fixed once content published
- handles delete storage without `downloadUrl`

# 4.0.0 (2021-12-17)

### Breaking Changes

- `publish` becomes `deckPublish` and `docPublish` (which is not supported by this provider)
- `dist-custom-elements` replaces `dist-custom-elements-bundle` output target

### Features

- publish documents
- upload assets of flat paragraphs (`deckgo-lazy-img` as paragraph themselves)
- in case an asset "resource" is updated, update service worker too

# 3.0.1 (2021-12-07)

### Fix

- URL token extraction to delete asset

# 3.0.0 (2021-12-03)

### Breaking Changes

- better handle bucket creation ("pending bucket")

### Features

- publish overview
- templates support
- lower case and dashed filename for storage

### Fix

- expose doc providers

# 2.1.0 (2021-11-28)

### Features

- publish overview (index.html) of all decks published

### Fix

- published decks attributes

# 2.0.1 (2021-11-27)

### Fix

- expose docs and paragraphs providers

# 2.0.0 (2021-11-27)

### Features

- one data canister per user for all decks and documents
- no more user canister
- storage enhanced with secret and public assets
- deck publish
- authentication reworked and delete user

# 1.0.0 (2021-10-21)

### Features

- Hello World 👋
