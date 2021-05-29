# 2.4.0 (2021-05-29)

### Build

- output custom elements as a single bundle (`dist-custom-elements-bundle`)
- bump dependencies

# 2.3.1 - 2.3.2 (2021-05-27)

### Fix

- reveal `highlight-code` selector

# 2.3.0 (2021-05-26)

### Build

- update dependencies including `slide-utils`

# 2.2.3 (2020-01-14)

### Fix

- new url `https://app.deckdeckgo.com/poll`

# 2.2.2 (2020-11-14)

### Fix

- cross-browser user selection

# 2.2.1 (2020-11-14)

### Fix

- publish to npm

# 2.2.0 (2020-11-14)

### Style

- default user select set to `text` instead of `none`

# 2.1.3 (2020-11-08)

### Features

- update slide-utils for lazy loading images

# 2.1.2 (2020-10-29)

### Fix

- update slide-utils for word-cloud

# 2.1.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

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
