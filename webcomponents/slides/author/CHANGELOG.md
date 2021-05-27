# 2.3.1 - 2.3.2 (2021-05-27)

### Fix

- reveal `highlight-code` selector

# 2.3.0 (2021-05-26)

### Build

- update dependencies including `slide-utils`

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

# 1.0.0-rc.3-2 (2020-01-09)

### Fix

- new slide design lazy loading (CSS was evaluating prior lazy loading)

# 1.0.0-rc.3-1 (2020-01-05)

### Features

- lazy load images on property update

# 1.0.0-rc.3 (2020-01-05)

### Features

- new design `cover` image (new default) and `none` option, in case you would like to not display an image. `circle` remains an option ([#558](https://github.com/deckgo/deckdeckgo/issues/558))
- new CSS variable `--slide-author-img-border` in case you would like to add a border around the `circle` image

# 1.0.0-rc.2 (2020-01-04)

### Features

- show handles in author slide ([#424](https://github.com/deckgo/deckdeckgo/issues/424))
- extract `@deckdeckgo/social` to a standalone component
