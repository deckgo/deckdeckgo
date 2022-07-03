# 0.0.33 (2022-07-02)

### Features

- update storage on delete only if needed

# 0.0.32 (2022-07-02)

### Build

- bump dependencies

# 0.0.31 (2022-07-01)

### Features

- update published landing page after remove a document or deck from storage

# 0.0.30 (2022-06-18)

### Features

- fallback file extension for download `.ddg` or `.papyrs`

# 0.0.29 (2022-06-12)

### Features

- `fetchAsset` from online and offline storage

### Fix

- import online assets in offline context

# 0.0.28 (2022-05-26)

### Features

- lazy load sub-images on tree mutation

# 0.0.27 (2022-05-20)

### Features

- clean zero width space before updating doc title

# 0.0.26 (2022-05-17)

### Fix

- preserve case sensitive keys when exporting local assets

# 0.0.25 (2022-05-12)

### Fix

- at runtime the store can contains `undefined` (resolve publish issue in DeckDeckGo `TypeError: Cannot set properties of undefined (setting 'name')`) 

# 0.0.24 (2022-05-04)

### Fix

- import Figma files backward compatibility

# 0.0.23 (2022-04-24)

### Features

- set doc name on update of first paragraph

# 0.0.22 (2022-04-14)

### Features

- clean more grammarly attributes

# 0.0.20 - 0.0.21 (2022-04-03)

### Features

- export file type

# 0.0.19 (2022-04-02)

### Features

- pass `config` to `docPublish`

# 0.0.17-0.0.18 (2022-03-26)

### Features

- clean node children

# 0.0.16 (2022-03-15)

### Features

- attach before unload window on busy

# 0.0.15 (2022-03-15)

### Fix

- `docSelector` match only parent `deckgo-studio-doc`

# 0.0.14 (2022-03-15)

### Features

- `docSelector` match parent `deckgo-studio-doc > article.deckgo-doc`

# 0.0.13 (2022-03-14)

### Fix

- catch sync errors and update status accordingly

# 0.0.12 (2022-03-04)

### Features

- admin services

# 0.0.11 (2022-03-03)

### Features

- storage provider

# 0.0.10 (2022-03-03)

### Fix

- `docSelector` match parent `deckgo-studio-doc`

# 0.0.9 (2022-03-03)

### Features

- `onNext` of snapshots as promise

# 0.0.8 (2022-03-02)

### Fix

- doc publish promise resolve

# 0.0.7 (2022-03-02)

### Features

- remove functions that are not part of sync

# 0.0.6 (2022-03-02)

### Features

- publish sync provider

# 0.0.5 (2022-02-25)

### Features

- user provider

# 0.0.4 (2022-02-25)

### Features

- fetch jszip from a cdn

# 0.0.3 (2022-02-25)

### Features

- import and export data to .ddg files

# 0.0.2 (2022-02-24)

### Build

- cjs

# 0.0.1 (2022-02-24)

### Features

- Hello World 👋
