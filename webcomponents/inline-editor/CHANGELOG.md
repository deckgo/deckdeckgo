# 3.2.1 (2021-05-08)

### Fix

- close inline editor in a shadowed context

# 3.2.0 (2021-05-07)

### Features

- preserve selection after applying style aka do not close inline editor

### Fix

- list manipulation

# 3.1.0 (2021-05-02)

### Features

- color picker with `input` field

# 3.0.2 (2020-10-29)

### Fix

- apply style if all list is selected

# 3.0.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 3.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 2.0.3 (2020-08-13)

### Fix

- `styleDidChange` was not always emitting the container

# 2.0.2 (2020-08-08)

### Features

- handles comment node

# 2.0.1 (2020-08-06)

### Style

- add a CSS4 variable `--deckgo-inline-editor-position`

# 2.0.0 (2020-08-06)

### Breaking Changes

- on style change the `input` event is not triggered anymore on the container. to overcome the problem, the component emits a new event `styleChange`
- the event `linkCreated` does not emit the link as detail anymore but rather the container

### Features

- replace `document.execCommand` marked as obsolete with custom code ([#741](https://github.com/deckgo/deckdeckgo/issues/741))

# 1.5.0 (2020-07-21)

### Features

- add new option `background-color`
- new icon for `color`

# 1.4.0 (2020-05-31)

### Features

- add strikethrough ([#710](https://github.com/deckgo/deckdeckgo/issues/710))

# 1.3.1 (2020-05-15)

### Fix

- color icon always default color to avoid white on white

# 1.3.0 (2020-05-15)

### Features

- update color component

# 1.2.1 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.2.0 (2020-04-30)

### Features

- modify the font-size (1-7)

# 1.1.1 (2020-04-18)

### Fix

- MIT icons which do not need attribution

# 1.1.0 (2020-04-17)

### Features

- add new actions to modify the alignment of the selection
- support RTL
- code refactored and split in separate components
- style review and enhancement

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

# 1.0.0-rc.3-3 (2019-12-12)

### Features

- add new property `palette` to allow customization of the palette of colors to be picked

# 1.0.0-rc.3-2 (2019-10-14)

### Fix

- on mouse selection the link were not correctly created as it kept a selection reference to the first or last character ([ #410](https://github.com/deckgo/deckdeckgo/issues/410)

# 1.0.0-rc.3-1 (2019-10-13)

### Fix

- create link don't always respect order ([#405](https://github.com/deckgo/deckdeckgo/issues/405))

# 1.0.0-rc.3 (2019-10-01)

### Features

- provide your own custom actions to the component. Kudos to [Matthias Max / Bitflower](https://github.com/bitflower) for the PR ([#380](https://github.com/deckgo/deckdeckgo/pull/380))

# 1.0.0-rc.2-1 (2019-09-21)

### Fix

- fix layout for color and link on sticky mobile devices

# 1.0.0-rc.2 (2019-09-21)

### Breaking Changes

- the component doesn't rely anymore on the platform's color picker but use the new `@deckdeckgo/color` Web Component to offer a limited and simple choice of selectable colors

# 1.0.0-rc.1-1 (2019-09-01)

### Fix

- debounce the display of the toolbar (useful in case of dragged selection)

# 1.0.0-rc.1 (2019-08-30)

### Features

- `@deckdeckgo/utils` isn't a singleton anymore but a collections of stateless functions

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-alpha.14 (2019-08-15)

- add an overall option to not display the toggle list options
- new style property `--deckgo-inline-editor-sticky-height)` for the toolbar height if sticky

# 1.0.0-alpha.13 (2019-08-09)

- trigger an event `linkCreated` when a link is, well, created

# 1.0.0-alpha.12 (2019-08-06)

- images will not be considered, per default, as editable anymore. to turn this feature on, use the property `img-editable`

# 1.0.0-alpha.11 (2019-07-30)

- fix close image processing toolbar if document or any other elements is clicked

# 1.0.0-alpha.10 (2019-06-09)

### Features

- display an icon instead of A for the color picker
- display elements not applicable per default hidden (instead of disabled)

# 1.0.0-alpha.9 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-alpha.8 (2019-05-30)

### Features

- sticky toolbar on iOS positioned at the top of the browser window
- improve behavior on mobile devices specially regarding selection and focus

### Lib

- use new package `@deckdeckgo/utils`

# 1.0.0-alpha.7 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/inline-editor`

# 1.0.0-alpha.6 (2019-05-16)

- this WYSIWYG inline editor now supports images too. it gives the ability to resize (big, medium, small, very small) images, to align them horizontally or left and even to delete them

# 1.0.0-alpha.5 (2019-04-30)

### Breaking change

- modify the default `containers` value from `slot` to `h1,h2,h3,h4,h5,h6,div`

# 1.0.0-alpha.4 (2019-04-18)

### Breaking changes

- this new version includes new features, fixes and breaking changes for the upcoming DeckDeckGo studio. Please refer to the documentation for its corresponding usage.
