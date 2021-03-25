# 3.2.3 (2021-03-25)

### Style

- use font size based on the width to preserve aspect ratio of texts 

# 3.2.2 (2020-11-14)

### Fix

- cross-browser user selection

# 3.2.1 (2020-11-14)

### Fix

- publish to npm

# 3.2.0 (2020-11-14)

### Style

- default user select set to `text` instead of `none`

# 3.1.3 (2020-11-08)

### Features

- update slide-utils for lazy loading images

# 3.1.2 (2020-10-29)

### Fix

- update slide-utils for word-cloud

# 3.1.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 3.1.0 (2020-09-24)

### Features

- support for `vertical` and `papyrus`
- update dev dependencies

# 3.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 2.0.1 (2020-07-12)

### Features

- pointer-events supported for `header` and `footer`

# 2.0.0 (2020-07-11)

### Breaking Changes

- in order to support `header` and `footer`, the slots duplicating these names add to be renamed to `top` and `bottom`

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

# 1.0.0-beta.1-2 (2020-02-27)

### Features

- rollback max-height in order to preserve ratio

# 1.0.0-beta.1-1 (2020-02-27)

### Features

- max height 100% for the aspect ratio content

# 1.0.0-beta.1 (2020-02-19)

### Features

- use this template to display a slide which preserves the same aspect ratio regardless of the devices ([#610](https://github.com/deckgo/deckdeckgo/issues/610))
