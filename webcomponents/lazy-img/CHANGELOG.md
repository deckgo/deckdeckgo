<a name="1.0.0-rc.1"></a>
# 1.0.0-rc.1 (2019-08-30)

### Libs

* update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

<a name="1.0.0-alpha.12"></a>
# 1.0.0-alpha.12 (2019-08-07)

### Features

* `lazy-img` component now lazy load images using native lazy-loading

<a name="1.0.0-alpha.11"></a>
# 1.0.0-alpha.11 (2019-08-06)

* add a CSS variable for the property `object-fit`

<a name="1.0.0-alpha.10"></a>
# 1.0.0-alpha.10 (2019-07-17)

* optionally, parse SVG instead of using such images in `<img/>` tags

<a name="1.0.0-alpha.9"></a>
# 1.0.0-alpha.9 (2019-07-06)

* add two new CSS variables for styling

<a name="1.0.0-alpha.8"></a>
# 1.0.0-alpha.8 (2019-06-27)

* don't display alt while loading
* add an `imgErrorSrc` which could be displayed in case the image would not be resolved

<a name="1.0.0-alpha.7"></a>
# 1.0.0-alpha.7 (2019-06-04)

### Libs

* update DeckDeckGo types last lib

<a name="1.0.0-alpha.6"></a>
# 1.0.0-alpha.6 (2019-06-04)

### Libs

* upgrade to Stencil One

<a name="1.0.0-alpha.5"></a>
# 1.0.0-alpha.5 (2019-05-30)

### Feature

* fallback on a good old standard display of the image (at component load time) in case of the `MutationObserver` would not be supported

<a name="1.0.0-alpha.4"></a>
# 1.0.0-alpha.4 (2019-05-24)

### Fix

* on attributes change, observe image again and fix new url

<a name="1.0.0-alpha.3"></a>
# 1.0.0-alpha.3 (2019-05-24)

### Breaking

* move to the org scoped package `@deckdeckgo/lazy-img`

### Features

* add a couple of CSS4 variables to style the image

<a name="1.0.0-alpha.2"></a>
# 1.0.0-alpha.2 (2019-05-18)

* add `srcset` and `sizes` support to the component
