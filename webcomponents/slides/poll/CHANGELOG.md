# 2.1.0 (2020-09-24)

### Features

- support for `vertical` and `papyrus`
- update dev dependencies

# 2.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 1.1.2 (2020-07-12)

### Features

- pointer-events supported for `header` and `footer`

# 1.1.1 (2020-07-11)

### Style

- adjusted `header` and `footer` positions

# 1.1.0 (2020-07-10)

### Features

- add `header` and `footer`

### Refactoring

- remove `custom-actions` and `custom-background` properties because these are only use as attributes

# 1.0.1 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.0.0 (2020-03-19)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-beta.5 (2019-12-04)

### Features

- Firebase can't redirect subdomain to path, therefore use `deckdeckgo.com/poll` instead of `poll.deckdeckgo.com`

# 1.0.0-beta.4 (2019-12-01)

### Breaking Changes

- update the poll displayed in the remote with the values of the deck (after the answers have been cast)
- `answeredOnce` as state

# 1.0.0-beta.3 (2019-12-01)

### Features

- expose `answeredOnce` as an attribute to sync the information between deck and remote

### Breaking Changes

- event `pollConnected` replaced by event `pollUpdated` triggered on each component updated

# 1.0.0-beta.2 (2019-12-01)

### Features

- remove RxJS and use callbacks instead

# 1.0.0-beta.1-1 (2019-11-30)

### Features

- add a CSS variable for the `font-size` of the slot `how-to`

<a name="1.0.0-beta.1"></a>

### Features

- interact with your audience with this new `poll` template ([#471](https://github.com/deckgo/deckdeckgo/issues/471))
