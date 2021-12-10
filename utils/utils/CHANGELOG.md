# 3.0.0 (2021-12-10)

### Breaking Changes

- color utils synchronous (not async anymore)

# 2.4.0 (2021-11-05)

### Features

- move cursor to start

### Fix

- lazy load all images async / await

# 2.3.0 (2021-10-29)

### Features

- content editable function to catch and insert tabs

# 2.2.0 (2021-10-27)

### Features

- new util functions to interact with the selection API

# 2.1.0 (2021-10-21)

### Fix

- exclude accurate pointing device (mouse) from `isMobile` detection

# 2.0.1 (2021-05-24)

### Fix

- main entry

# 2.0.0 (2021-05-15)

### Breaking Changes

- build to ECMAScript module (`esm`)
- output `esnext`
- drop `iife` and `cjs` format

### Build

- migrate to `esbuild`

<a name="1.8.1"></a>

# 1.8.1 (2021-04-23)

### Chore

- remove document check

<a name="1.8.0"></a>

# 1.8.0 (2021-01-30)

### Features

- `isAndroid`
- `isAndroidTablet`
- use `fullscreenElement` (with prefix) to detect full screen

<a name="1.7.0"></a>

# 1.7.0 (2020-11-21)

### Features

- isLandscape and isPortrait

<a name="1.6.0"></a>

# 1.6.0 (2020-11-20)

### Fix

- is mobile based on matchMedia
- isIos including new iPads

<a name="1.5.0"></a>

# 1.5.0 (2020-11-20)

### Fix

- update is mobile

<a name="1.4.0"></a>

# 1.4.0 (2020-11-20)

### Fix

- detection iPad for iOS >= 13

<a name="1.3.0"></a>

# 1.3.0 (2020-08-15)

- improve `extractRgb` to support decimals value (as for example `rgb(5.5, 4.7, 4)`)
- expose function `extractRgb`
- add and expose function `extractRgba`

<a name="1.2.0"></a>

# 1.2.0 (2020-07-31)

### Features

- add functions `hexToRgb` and `rgbToHex`

<a name="1.1.0"></a>

# 1.1.0 (2020-04-17)

### Features

- add function `isRTL()`

<a name="1.0.0"></a>

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

<a name="1.0.0-rc.3"></a>

# 1.0.0-rc.3 (2020-02-23)

### Features

- add Cory McArthur's handy `injectCSS` and `injectJS` functions to utils

<a name="1.0.0-rc.2"></a>

# 1.0.0-rc.2 (2019-11-29)

### Features

- bundle package with rollup

<a name="1.0.0-rc.1-4"></a>

# 1.0.0-rc.1-4 (2019-11-13)

### Features

- add function `isFirefox`

<a name="1.0.0-rc.1-3"></a>

# 1.0.0-rc.1-3 (2019-11-05)

### Refactoring

- source moved and renamed (no functional changes)

<a name="1.0.0-rc.1-2"></a>

# 1.0.0-rc.1-2 (2019-09-21)

### Features

- add `isIPad()` function

<a name="1.0.0-rc.1-1"></a>

# 1.0.0-rc.1-1 (2019-09-01)

### Features

- improve `debounce` typing

<a name="1.0.0-rc.1"></a>

# 1.0.0-rc.1 (2019-08-30)

### Features

- library isn't a singleton anymore but a collections of stateless functions

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)
