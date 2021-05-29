# 4.0.3 (2021-05-29)

### Fix

- remove `console.log`

# 4.0.2 (2021-05-27)

### Fix

- reveal / animate highlight code component only if lines have to be highlighted

# 4.0.1 (2021-05-24)

### Fix

- main entry

# 4.0.0 (2021-05-21)

### Breaking Changes

- reveal state interface modified (renamed to `revealProgress`, type instead of boolean)

# 3.0.0 (2021-05-16)

### Breaking Changes

- build to ECMAScript module (`esm`)
- output `esnext`
- drop `iife` and `cjs` format

### Build

- migrate to `esbuild`

<a name="2.6.1"></a>

# 2.6.1 (2020-11-14)

### Style

- cross-browser user selection

<a name="2.6.0"></a>

# 2.6.0 (2020-11-14)

### Style

- templates user select set to `text`

<a name="2.5.0"></a>

# 2.5.0 (2020-11-12)

- lazy load new `@deckgo/markdown` component in presentations

<a name="2.4.0"></a>

# 2.4.0 (2020-11-08)

### Features

- lazy load all img and deckgo/lazy-img children

<a name="2.3.0"></a>

# 2.3.0 (2020-10-28)

### Features

- lazy load word-clouds

<a name="2.2.1"></a>

# 2.2.1 (2020-09-20)

### Style

- modify `min-height` for `papyrus`

<a name="2.2.0"></a>

# 2.2.0 (2020-09-19)

### Features

- default overflow `hidden` introduced to support `vertical` swipe
- update dependencies to latest

<a name="2.1.2"></a>

# 2.1.2 (2020-07-12)

### Features

- pointer-events per default supported for header and footer

<a name="2.1.1"></a>

# 2.1.1 (2020-07-11)

### Fix

- `z-index` between `header`, `footer`, content and backgrounds

<a name="2.1.0"></a>

# 2.1.0 (2020-07-11)

### Features

- default `top` and `bottom` padding when screen < `1024px` set to `32px` per default
- smaller margin and size for the `header` and `footer`

<a name="2.0.0"></a>

# 2.0.0 (2020-07-10)

### Breaking Changes

- move `slot` related styles to a separate new style sheet `deckdeckgo-slide-slots.scss`

### Features

- add style for `header` and `footer`

<a name="1.1.0"></a>

# 1.1.0 (2020-05-07)

### Features

- add new component `deckgo-demo` to the list of component to lazy load

<a name="1.0.0"></a>

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

<a name="1.0.0-rc.4"></a>

# 1.0.0-rc.4 (2019-11-29)

### Features

- bundle package with rollup

<a name="1.0.0-rc.3-2"></a>

# 1.0.0-rc.3-2 (2019-11-05)

### Lib

- update to last DeckDeckGo utils dependencies

<a name="1.0.0-rc.3-1"></a>

# 1.0.0-rc.3-1 (2019-11-05)

### Refactoring

- source moved and renamed (no functional changes)

<a name="1.0.0-rc.3"></a>

# 1.0.0-rc.3 (2019-10-07)

### Features

- new interface `DeckdeckgoSlideResize`

<a name="1.0.0-rc.2"></a>

# 1.0.0-rc.2 (2019-09-14)

### Features

- new interface `DeckdeckgoSlidePlay`
