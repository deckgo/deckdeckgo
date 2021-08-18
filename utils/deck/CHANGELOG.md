# 4.1.0 (2021-08-18)

### Features

- `findSlidesTitle` deck query selector can be specified

# 4.0.2 (2021-05-24)

### Fix

- main entry
  
<a name="4.0.1"></a>

# 4.0.1 (2021-05-15)

### Build

- remove unused dependencies

<a name="4.0.0"></a>

# 4.0.0 (2021-05-15)

### Breaking Changes

- build to ECMAScript module (`esm`)
- drop `iife` and `cjs` formats

### Build

- migrate to `esbuild`

### Style

- default `h1` font-size `3em` regardless of the screen size

<a name="3.4.1"></a>

# 3.4.1 (2021-03-25)

### Style

- preserve slide `aspect-ratio` background aspect with `papyrus` too

<a name="3.4.0"></a>

# 3.4.0 (2021-02-19)

### Features

- generic slides detection

<a name="3.3.1"></a>

# 3.3.1 (2021-01-30)

### Style

- default `slot` padding to `8px`

<a name="3.3.0"></a>

# 3.3.0 (2020-11-29)

### Features

- clean `spellcheck` with `cleanContent` function

<a name="3.2.1"></a>

# 3.2.1 (2020-11-15)

### Style

- markdown style is shadowed

<a name="3.2.0"></a>

# 3.2.0 (2020-11-14)

### Style

- Firefox workaround

<a name="3.1.0"></a>

# 3.1.0 (2020-11-12)

### Features

- improvements for the integration of the new `@deckgo/markdown` component in our editor

<a name="3.0.2"></a>

# 3.0.2 (2020-10-29)

### Style

- papyrus slotted image height

<a name="3.0.1"></a>

# 3.0.1 (2020-10-23)

### Fix

- lazy img should remain applied with a scroll auto

<a name="3.0.0"></a>

# 3.0.0 (2020-10-23)

### Features

- the presentation base font is responsive and applied by the core according screen height
- editable content is resizing according users' entries (no overflow scroll)

<a name="2.8.0"></a>

# 2.8.0 (2020-10-18)

### Features

- position of waves in background

<a name="2.7.0"></a>

# 2.7.0 (2020-10-13)

### Features

- clean attribute `editable` when `false` too

<a name="2.6.0"></a>

# 2.6.0 (2020-09-23)

### Style

- style according `papyrus` is now going to be applied on a class selector

<a name="2.5.1"></a>

# 2.5.1 (2020-09-20)

### Style

- fits `cover` correctly

<a name="2.5.0"></a>

# 2.5.0 (2020-09-20)

### Style

- background lazy-img fits `cover` for `papyrus`

### Features

- update dependencies

<a name="2.4.0"></a>

# 2.4.0 (2020-08-06)

### Features

- adapt `font-weight` to work with new inline editor

<a name="2.3.3"></a>

# 2.3.3 (2020-07-21)

### Fix

- inherit `font-weight` in titles and strong when top is not set as strong

<a name="2.3.2"></a>

# 2.3.2 (2020-07-21)

### Fix

- inherit `font-weight` in titles and strong

<a name="2.3.1"></a>

# 2.3.1 (2020-06-13)

### Style

- improve <li/> spacing, width and font size if reveal

<a name="2.3.0"></a>

# 2.3.0 (2020-05-14)

### Features

- add style for new component `deckgo-math`

<a name="2.2.0"></a>

# 2.2.0 (2020-05-07)

### Features

- add style for new component `deckgo-demo` in `split` template

<a name="2.1.1"></a>

# 2.1.1 (2020-05-03)

### Fix

- split template column align per default is start

<a name="2.1.0"></a>

# 2.1.0 (2020-05-03)

### Fix

- `deckgo-slide-split` alignment in case of children (`b`, `i`, etc.)
- `line-height` was too big in cas a small font size would be used

<a name="2.0.0"></a>

# 2.0.0 (2020-05-01)

### Features

- apply big `font-size` on the deck only in fullscreen
- define `font-size` if the user's modify size inside sections
- deck `h1` and `h2` bolder
- use Saas to compiles styles

<a name="1.0.1"></a>

# 1.0.1 (2020-04-18)

### Features

- `split` template horizontal with default alignment

<a name="1.0.0"></a>

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

<a name="1.0.0-rc.3-3"></a>

# 1.0.0-rc.3-3 (2020-03-09)

- padding around social logo on Windows Chrome

<a name="1.0.0-rc.3-2"></a>

# 1.0.0-rc.3-2 (2020-02-23)

<a name="1.0.0-rc.3-1"></a>

# 1.0.0-rc.3-1 (2020-02-20)

- style `deckgo-lazy-img` attribute `object-fit` in new component `deckgo-drr`

<a name="1.0.0-rc.3"></a>

# 1.0.0-rc.3 (2020-02-19)

- style `deckgo-lazy-img` when used in new component `deckgo-drr`

<a name="1.0.0-rc.2-3"></a>

# 1.0.0-rc.2-3 (2020-01-20)

- add slide poll question to list of slides titles

<a name="1.0.0-rc.2-2"></a>

# 1.0.0-rc.2-2 (2020-01-16)

### Features

- clean `custom-loader` attribute

<a name="1.0.0-rc.2-1"></a>

# 1.0.0-rc.2-1 (2020-01-04)

### Features

- align icon center for social link

<a name="1.0.0-rc.2"></a>

# 1.0.0-rc.2 (2019-11-29)

### Features

- add `getSlideDefinition` and `getAttributesDefinition` functions to deck utils as these are notably used in the editor now too
- bundle package with rollup

<a name="1.0.0-rc.1-2"></a>

# 1.0.0-rc.1-2 (2019-11-13)

### Fix

- notes slot were displayed on template "split"

<a name="1.0.0-rc.1-1"></a>

# 1.0.0-rc.1-1 (2019-11-08)

### Fix

- improve regex of the `cleanContent` function in order to remove only attributes not content/text

<a name="1.0.0-rc.1"></a>

# 1.0.0-rc.1 (2019-11-05)

### Features

- initial release to sync the content between decks and the remote control
