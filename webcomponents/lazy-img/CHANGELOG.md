# 3.0.0 (2020-11-08)

### Breaking Changes

Per default, the component uses the intersection observer to lazy load the images.
If set to `lazy` then it will defer the loading using the native web lazy loading.

### Features

- introduces a new property `loading` set to `eager` per default
- more sensitive default `observer-threshold` and `observer-root-margin`

# 2.0.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 2.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 1.0.1 (2020-05-11)

### Feat

- update Stencil for Gatsby build

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-rc.3-2 (2020-02-19)

### Features

- `svg` container size inherit content

# 1.0.0-rc.3-1 (2020-01-30)

### Features

- new CSS variable to apply a `box-shadow` to the lazy loaded image

# 1.0.0-rc.3 (2020-01-15)

### Features

- attach a custom loader to display the image once lazy loaded (new option)

# 1.0.0-rc.2-3 (2019-12-13)

### Fix

- svg weren't lazy loaded anymore on browsers supporting native lazy loading

# 1.0.0-rc.2-2 (2019-12-13)

### Fix

- svg weren't displayed anymore since v1.0.0-rc.2

# 1.0.0-rc.2-1 (2019-11-29)

### Features

- add new optional properties `img-width` and `img-height` (useful for native lazy loading in Chrome)

# 1.0.0-rc.2 (2019-11-29)

### Features

- display smoothly the images once loaded ([#508](https://github.com/deckgo/deckdeckgo/issues/508))

# 1.0.0-rc.1 (2019-08-30)

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-alpha.12 (2019-08-07)

### Features

- `lazy-img` component now lazy load images using native lazy-loading

# 1.0.0-alpha.11 (2019-08-06)

- add a CSS variable for the property `object-fit`

# 1.0.0-alpha.10 (2019-07-17)

- optionally, parse SVG instead of using such images in `<img/>` tags

# 1.0.0-alpha.9 (2019-07-06)

- add two new CSS variables for styling

# 1.0.0-alpha.8 (2019-06-27)

- don't display alt while loading
- add an `imgErrorSrc` which could be displayed in case the image would not be resolved

# 1.0.0-alpha.7 (2019-06-04)

### Libs

- update DeckDeckGo types last lib

# 1.0.0-alpha.6 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-alpha.5 (2019-05-30)

### Feature

- fallback on a good old standard display of the image (at component load time) in case of the `MutationObserver` would not be supported

# 1.0.0-alpha.4 (2019-05-24)

### Fix

- on attributes change, observe image again and fix new url

# 1.0.0-alpha.3 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/lazy-img`

### Features

- add a couple of CSS4 variables to style the image

# 1.0.0-alpha.2 (2019-05-18)

- add `srcset` and `sizes` support to the component
