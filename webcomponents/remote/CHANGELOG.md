# 2.2.0 (2021-05-29)

### Build

- output custom elements as a single bundle (`dist-custom-elements-bundle`)

# 2.1.0 (2021-05-28)

### Build

- bump dependencies

# 2.0.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 2.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 1.2.0 (2020-06-30)

### Features

- migrate `rxjs` to `@stencil/store` ([#773](https://github.com/deckgo/deckdeckgo/issues/773))

# 1.1.1 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.1.0 (2020-04-10)

### Features

- update dependencies and `ConnectionState` enum definition in types

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-rc.6 (2020-02-22)

### Features

- draw arrows ([#625](https://github.com/deckgo/deckdeckgo/issues/625))

# 1.0.0-rc.5 (2019-11-30)

### Breaking Changes

- property `server` renamed to `socketUrl`

# 1.0.0-rc.4-1 (2019-11-05)

### Fix

- clear draw history

# 1.0.0-rc.4 (2019-11-05)

### Breaking Changes

- update component in order to sync content between decks and remote

### Features

- add a method to update a particular slide
- add a method to update the "reveal" settings of the deck of the remote control

# 1.0.0-rc.3 (2019-10-25)

### Features

- drawing circle clear the all pane ([#66](https://github.com/deckgo/deckdeckgo/issues/66))

# 1.0.0-rc.2 (2019-09-15)

### Features

- add methods to forward play and pause from the deck to the remote app

# 1.0.0-rc.1-1 (2019-09-15)

### Libs

- update `@deckdeckgo/types` last definition

# 1.0.0-rc.1 (2019-08-30)

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-alpha.11 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-alpha.10 (2019-05-26)

### Libs

- use org scoped `@deckdeckgo/types`

# 1.0.0-alpha.9 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/remote`

# 1.0.0-alpha.8 (2019-04-18)

### Features

- add methods `updateSlides` and `deleteSlide`

# 1.0.0-alpha.7 (2019-04-14)

### Features

- remote canvas positioned absolutely in order to inherit parent container

# 1.0.0-alpha.6 (2019-04-14)

### Features

- add an attribute `autoConnect` if you don't wish the component to establish a connection on load
- add a `disconnect` method

# 1.0.0-alpha.5 (2019-02-02)

### Lib

- update Stencil and other libs ([758ea62](https://github.com/deckgo/deckdeckgo-remote/commit/758ea62c884a67195753f805088d11e43a516873))

# 1.0.0-alpha.4 (2019-01-15)

### Features

- include new types with slide definition ([#3](https://github.com/deckgo/deckdeckgo-remote/issues/3))

# 1.0.0-alpha.3 (2018-12-05)

### Breaking Changes

- property `slides` has been renamed to `length` ([#1](https://github.com/deckgo/deckdeckgo-remote/issues/1))

### Features

- add a new property `slides` to transmit the ordered list of slides ([#2](https://github.com/deckgo/deckdeckgo-remote/issues/2))
