<a name="1.1.0"></a>

# 1.1.0 (2020-04-17)

### Features

- add new actions to modify the alignment of the selection
- support RTL
- code refactored and split in separate components
- style review and enhancement

<a name="1.0.0"></a>

# 1.0.0 (2020-03-18)

To infinity and beyond 🚀

### Features

- update dependencies

<a name="1.0.0-rc.3-3"></a>

# 1.0.0-rc.3-3 (2019-12-12)

### Features

- add new property `palette` to allow customization of the palette of colors to be picked

<a name="1.0.0-rc.3-2"></a>

# 1.0.0-rc.3-2 (2019-10-14)

### Fix

- on mouse selection the link were not correctly created as it kept a selection reference to the first or last character ([ #410](https://github.com/deckgo/deckdeckgo/issues/410)

<a name="1.0.0-rc.3-1"></a>

# 1.0.0-rc.3-1 (2019-10-13)

### Fix

- create link don't always respect order ([#405](https://github.com/deckgo/deckdeckgo/issues/405))

<a name="1.0.0-rc.3"></a>

# 1.0.0-rc.3 (2019-10-01)

### Features

- provide your own custom actions to the component. Kudos to [Matthias Max / Bitflower](https://github.com/bitflower) for the PR ([#380](https://github.com/deckgo/deckdeckgo/pull/380))

<a name="1.0.0-rc.2-1"></a>

# 1.0.0-rc.2-1 (2019-09-21)

### Fix

- fix layout for color and link on sticky mobile devices

<a name="1.0.0-rc.2"></a>

# 1.0.0-rc.2 (2019-09-21)

### Breaking Changes

- the component doesn't rely anymore on the platform's color picker but use the new `@deckdeckgo/color` Web Component to offer a limited and simple choice of selectable colors

<a name="1.0.0-rc.1-1"></a>

# 1.0.0-rc.1-1 (2019-09-01)

### Fix

- debounce the display of the toolbar (useful in case of dragged selection)

<a name="1.0.0-rc.1"></a>

# 1.0.0-rc.1 (2019-08-30)

### Features

- `@deckdeckgo/utils` isn't a singleton anymore but a collections of stateless functions

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

<a name="1.0.0-alpha.14"></a>

# 1.0.0-alpha.14 (2019-08-15)

- add an overall option to not display the toggle list options
- new style property `--deckgo-inline-editor-sticky-height)` for the toolbar height if sticky

<a name="1.0.0-alpha.13"></a>

# 1.0.0-alpha.13 (2019-08-09)

- trigger an event `linkCreated` when a link is, well, created

<a name="1.0.0-alpha.12"></a>

# 1.0.0-alpha.12 (2019-08-06)

- images will not be considered, per default, as editable anymore. to turn this feature on, use the property `img-editable`

<a name="1.0.0-alpha.11"></a>

# 1.0.0-alpha.11 (2019-07-30)

- fix close image processing toolbar if document or any other elements is clicked

<a name="1.0.0-alpha.10"></a>

# 1.0.0-alpha.10 (2019-06-09)

### Features

- display an icon instead of A for the color picker
- display elements not applicable per default hidden (instead of disabled)

<a name="1.0.0-alpha.9"></a>

# 1.0.0-alpha.9 (2019-06-04)

### Libs

- upgrade to Stencil One

<a name="1.0.0-alpha.8"></a>

# 1.0.0-alpha.8 (2019-05-30)

### Features

- sticky toolbar on iOS positioned at the top of the browser window
- improve behavior on mobile devices specially regarding selection and focus

### Lib

- use new package `@deckdeckgo/utils`

<a name="1.0.0-alpha.7"></a>

# 1.0.0-alpha.7 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/inline-editor`

<a name="1.0.0-alpha.6"></a>

# 1.0.0-alpha.6 (2019-05-16)

- this WYSIWYG inline editor now supports images too. it gives the ability to resize (big, medium, small, very small) images, to align them horizontally or left and even to delete them

<a name="1.0.0-alpha.5"></a>

# 1.0.0-alpha.5 (2019-04-30)

### Breaking change

- modify the default `containers` value from `slot` to `h1,h2,h3,h4,h5,h6,div`

<a name="1.0.0-alpha.4"></a>

# 1.0.0-alpha.4 (2019-04-18)

### Breaking changes

- this new version includes new features, fixes and breaking changes for the upcoming DeckDeckGo studio. Please refer to the documentation for its corresponding usage.
