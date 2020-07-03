# 1.1.2 (2020-07-03)

### Style

- set default scrollbar (overflow) behavior to `auto` instead of `scroll`
- host `display` set as `block`

# 1.1.1 (2020-06-14)

### Style

- line numbers color inherits code's comments color
- when highlighting, highlight only code not line numbers

# 1.1.0 (2020-05-21)

### Features

- themes for the carbon terminals

# 1.0.5 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.0.4 (2020-04-27)

### Fix

- if prism script did not load, do not keep reference in DOM

# 1.0.3 (2020-04-26)

### Features

- make code white-space a css variable ([#707](https://github.com/deckgo/deckdeckgo/issues/707))

# 1.0.2 (2020-04-02)

### Fix

- PrismJS CSS import into bundle

# 1.0.1 (2020-03-21)

### Features

- update dependencies

### Fix

- republish component bundle to fix loading of '~prismjs/themes/prism.css'

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-rc.3-2 (2020-02-23)

### Fix

- CSS typo

# 1.0.0-rc.3-1 (2020-02-23)

- CSS variable misspelled
- require @deckdeckgo/utils rc.3

# 1.0.0-rc.3 (2020-02-23)

### Breaking Changes

- property `carbon` renamed to `terminal` (default display remains the same)

### Features

- new optional terminal style "Ubuntu" ([#623](https://github.com/deckgo/deckdeckgo/issues/623))

# 1.0.0-rc.2-1 (2019-12-12)

### Features

- if the language definition doesn't exist or if unpkg is down, display code anyway

# 1.0.0-rc.2 (2019-12-12)

### Breaking Changes

- as of now, code will be per default displayed in a stylish card. you could opt out setting the property `carbon` to `false` ([#525](https://github.com/deckgo/deckdeckgo/issues/525))

# 1.0.0-rc.1-3 (2019-10-21)

### Fix

- missing place holder (not displayed)

# 1.0.0-rc.1-2 (2019-10-21)

### Fix

- wrong code highlight when line numbers are off ([#423](https://github.com/deckgo/deckdeckgo/issues/423))

# 1.0.0-rc.1-1 (2019-09-09)

### Features

- display code container as `block` per default

# 1.0.0-rc.1 (2019-08-30)

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-alpha.19 (2019-08-15)

- add some CSS4 variables to style the code's container

# 1.0.0-alpha.18 (2019-08-09)

## Features

- improve lines highlighting detection with ResizeObserver for modern browsers

## Fix

- the component displayed line numbers from 1 but the highlight used to expect line numbers from 0. this is now fixed, highlight will also begin with 1.
- add empty lines if line numbers has to be displayed to avoid sizing problem regarding font-size

# 1.0.0-alpha.17 (2019-07-30)

### Features

- display optional line numbers [#115](https://github.com/deckgo/deckdeckgo/issues/115)

# 1.0.0-alpha.16 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-alpha.15 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/highlight-code`

# 1.0.0-alpha.14 (2019-04-30)

### Features

- add a default color for the code highlighting background

# 1.0.0-alpha.13 (2019-04-30)

### Features

- add new properties and events in case you would like optionally set the code block as `editable`

# 1.0.0-alpha.12 (2019-03-03)

### Features

- style: add a variable for the display of the shadowed code element ([e00a3ea](https://github.com/deckgo/deckdeckgo/commit/e00a3ea170330de867cfedcdd0390c56988ae2f7))

# [1.0.0-alpha.11](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.10...v1.0.0-alpha.11) (2019-02-11)

### Breaking changes

- refactor CSS4 margin variable to give access to the all margin element not just margin-bottom ((ec4d260)[https://github.com/deckgo/deckdeckgo-highlight-code/commit/ec4d2602d3fa10e8e1ddf0ee20790d1fd6646720])

# [1.0.0-alpha.10](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.9...v1.0.0-alpha.10) (2019-02-01)

### Features

- add a method to load or reload the component ([ab11a8c](https://github.com/deckgo/deckdeckgo-highlight-code/commit/ab11a8c3be46bb7b6f3cdee6d9c7b0119baea595))
- loading of component with src and particular language ([ed88c8e](https://github.com/deckgo/deckdeckgo-highlight-code/commit/ed88c8e92aceebeb7d673819a8bed616d8ed8022))
- prevent error in case language is set as empty ([3790fc9](https://github.com/deckgo/deckdeckgo-highlight-code/commit/3790fc94da019047df7792628478f7a7d0c390d5))

# [1.0.0-alpha.9](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.8...v1.0.0-alpha.9) (2019-02-01)

### Fix and rollback

- if not shadowed the component was not compatible if used in other shadowed components, therefore rollback to a shadowed component ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

### Features

- add height new CSS4 variables to style the highlighted code ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

# [1.0.0-alpha.8](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.8...v1.0.0-alpha.8) (2019-02-01)

### Fix

- when the component was used in shadowed component, sometimes the slot content was not loaded ([6df0d0e](https://github.com/deckgo/deckdeckgo-highlight-code/commit/6df0d0e6802afeba2277caa90b1b0e4ddcc39fed))

# [1.0.0-alpha.7](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.6...v1.0.0-alpha.7) (2019-02-01)

### Features

- this Web Component isn't shadowed anymore to give you full access to its style ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

### Fix

- synchronize load languages across multiple components ([44fac83](https://github.com/deckgo/deckdeckgo-highlight-code/commit/44fac8378d6ea0d88ccb67175e3dcca2f0b9bb52))

# [1.0.0-alpha.6](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.5...v1.0.0-alpha.6) (2019-01-31)

### Refactor

- export anchor interface declaration ([680a2d3](https://github.com/deckgo/deckdeckgo-highlight-code/commit/680a2d30746a9cb86c617433b46ccb5c30894491))

# [1.0.0-alpha.5](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.4...v1.0.0-alpha.5) (2019-01-23)

### Bugs

- highlight code lines: first element might be a text ([#4](https://github.com/deckgo/deckdeckgo-highlight-code/issues/4))

# [1.0.0-alpha.4](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.3...v1.0.0-alpha.4) (2019-01-23)

### Bugs

- slot code: unescape characters ([#2](https://github.com/deckgo/deckdeckgo-highlight-code/issues/2))
- CSS text-align per default left ([#3](https://github.com/deckgo/deckdeckgo-highlight-code/issues/3))

# [1.0.0-alpha.3](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.2...v1.0.0-alpha.3) (2019-01-20)

### Features

- highlight code lines ([#1](https://github.com/deckgo/deckdeckgo-highlight-code/issues/1))

# [1.0.0-alpha.2](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.1...v1.0.0-alpha.2) (2019-01-20)

### Breaking changes

- this Web Component doesn't provide a scroll assist method anymore but rather provide a method `findNextAnchor()` which would provide you the information about the next anchor position to scroll to
