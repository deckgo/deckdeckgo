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

# 1.0.3 (2020-05-29)

### Fix

- QR code size calculation, again ([#743](https://github.com/deckgo/deckdeckgo/issues/743))

# 1.0.2 (2020-05-29)

### Fix

- QR code size calculation ([#743](https://github.com/deckgo/deckdeckgo/issues/743))

# 1.0.1 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.0.0 (2020-03-19)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-rc.2-1 (2019-10-12)

### Fix

- don't set a default 100% value as it might be to wide depending on the slide format

# 1.0.0-rc.2 (2019-10-07)

### Features

- `--slide-qrcode-title-display` default set to `block`
- reflect to attribute thee property `content`
- implement new interface `DeckdeckgoSlideResize`
- add a new property to display a logo over the QR code
- on `componentDidUpdate()` lazy load the logo too
