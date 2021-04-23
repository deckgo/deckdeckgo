# 8.1.5 (2021-04-23)

### Style

- on cursor active `inherit` instead of `initial` behavior

# 8.1.4 (2021-03-25)

### Style

- set a CSS font size for aspect ratio slides

# 8.1.3 (2021-02-17)

### Fix

- support swipe for templates with custom names

# 8.1.2 (2021-01-30)

### Fix

- slide might not be yet defined

# 8.1.1 (2020-11-14)

### Fix

- cross browser `user-select` detection

# 8.1.0 (2020-11-14)

### Features

- no swipe on user select text elements

# 8.0.2 (2020-11-08)

### Fix

- intersection observer threshold to lazy load next slides content

# 8.0.1 (2020-11-08)

### Features

- intersection observer more sensitive

### Fix

- no re-render on length change
- unobserve if observer already defined

# 8.0.0 (2020-10-28)

### Features

- move reveal to standalone components

# 7.0.0 (2020-10-23)

### Features

- apply a responsive base font size on the deck according screen height (can be override with a CSS4 variable)

# 6.1.2 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 6.1.1 (2020-10-21)

### Features

- set position main container after slideTo

# 6.1.0 (2020-10-17)

### Features

- on fullscreen enter, hide mouse more quickly

# 6.0.0 (2020-10-03)

### Breaking Changes

- `pager` is not part of the core anymore but shipped as a separate new component `deckgo-pager`

# 5.1.3 (2020-10-01)

### Features

- revert auto slide props reflected to attributes (the remote receive swipe information without these)

# 5.1.2 (2020-10-01)

### Features

- auto slide pros set as string enum as boolean are not conveniently reflected to attributes for our construct

### Fix

- auto slide active index

# 5.1.1 (2020-10-01)

### Features

- reflect auto slide properties to attributes

# 5.1.0 (2020-10-01)

### Features

- auto slide

# 5.0.1 (2020-09-26)

### Fix

- `direction-mobile` not used to initialize the size of the deck

# 5.0.0 (2020-09-26)

### Breaking Changes

- replace custom lazy loading implementation, based on slide index, with the IntersectionObserver API

### Fix

- improve boot performance by better detecting when all slides effectively loaded

# 4.0.3 (2020-09-24)

### Fix

- Hack for Google Chrome until the `smooth` scroll behavior is going to be supported if triggered with keyboard or programmatically

# 4.0.2 (2020-09-24)

### Style

- `slide-height` set to `100%` instead of `auto` in mode `papyrus`

# 4.0.1 (2020-09-24)

### Features

- rename also optional CSS variables prefixed with `transition-` to `animation-`

# 4.0.0 (2020-09-24)

### Breaking Changes

- `transition` property has been renamed to `animation`
- `isMobile` method removed. use the exposed function of `@deckdeckgo/utils` instead.

### Features

- introduce the ability to choose a specific `direction` for mobile devices
- reflect `direction` to a respective host class

# 3.0.0 (2020-09-20)

### Breaking Changes

- the deck now contains a new shadowed `main` element set to a position `absolute` which might conflict with previous usages

### Features

- swipe `vertical` and `papyrus`

# 2.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

# 1.2.1 (2020-08-07)

### Style

- pager display `none` not `hidden` (I need a coffee)

# 1.2.0 (2020-08-07)

### Style

- pager default display set to `none`. if you wish to display it, use the variable `--pager-display`.
- if displayed, pager color does not change according some background color anymore. use CSS4 variable to customize it for your slides.
- pager progression text default color set to `black`

# 1.1.0 (2020-07-10)

### Features

- introduces methods to load `header` and `footer`

### Style

- pager default size reduced to `48px`

# 1.0.2 (2020-05-11)

### Features

- update Stencil for Gatsby build

# 1.0.1 (2020-03-18)

### Features

- update dependencies

# 1.0.0 (2020-03-17)

To infinity and beyond 🚀

### Features

- update Stencil v1.10

# 1.0.0-rc.4 (2020-03-07)

### Features

- expose method `lazyLoadAllContent()`

# 1.0.0-rc.3-5 (2020-01-20)

### Features

- emit an event "deckDidLoad" after all slides have been loaded and processed

# 1.0.0-rc.3-4 (2019-12-13)

### Features

- don't drag slide if transition is not "slide"

# 1.0.0-rc.3-3 (2019-12-10)

### Features

- add and inherit a CSS variable --slide-transition

# 1.0.0-rc.3-2 (2019-12-10)

### Features

- limit `fade` transition effect to opacity (instead of all)

# 1.0.0-rc.3-1 (2019-12-10)

### Features

- improve fade transition flash

### Fix

- modify transition on the fly (reset transition duration)

# 1.0.0-rc.3 (2019-12-10)

### Features

- add a `transition` option. available animations are `slide` (default), `fade` or `none`

# 1.0.0-rc.2-1 (2019-11-30)

### Features

- `getSlideDefinition` and `getAttributesDefinition` functions moved to `@deckdeckgo/deck-utils`

# 1.0.0-rc.2 (2019-11-05)

### Breaking Changes

- `getSlidesDefinition` replaced by `getDeckDefinition` with the goal to sync content between decks and remote control

### Features

- new events `slideNextDidAnimate` and `slidePrevDidAnimate`
- new property `revealOnMobile` in case you would like to use the animation on mobile too
- new method `getSlideDefinition(index)` to retrieve the definition of a particular slide, used to sync content between decks and remote

# 1.0.0-rc.1-3 (2019-11-03)

### Features

- update keyboard assist to support PageUp/PageDown

### Libs

- update to most recent dependencies

# 1.0.0-rc.1-2 (2019-10-07)

### Features

- update keyboard assist to support Vim key bindings (`k` and `j` to swipe slides too)

# 1.0.0-rc.1-1 (2019-09-04)

### Features

- slide height on mobile devices ([#300](https://github.com/deckgo/deckdeckgo/issues/300))

# 1.0.0-rc.1 (2019-08-30)

### Breaking Changes

- extract slides and extra components to separate components ([#230](https://github.com/deckgo/deckdeckgo/issues/230))

#### But why?

These are the reasons behind the above breaking change:

- more flexibility

- better performances

With this change, you will now be able to include in your web project only the selected slides/templates you would like.
Doing so, even if already every web components were lazy loaded and only loaded when needed, this could now help to even minimize the dependencies and therefore reduce the size of the bundle of your application.

Note that per default, the `starter kit` will embed all slides/templates, therefore this change is going to be transparent for all new presentations you would create.

### Features

- `pager` color per default black
- `pager` change its color, if not explicitly specified, to match the `--background` color of the `slide` or the `deck` if provided

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-beta.46 (2019-08-15)

### Breaking Changes

- add new components `<deckgo-reveal/>` and `<deckgo-reveal-list/>` to make elements and texts appear one line at a time ([#224](https://github.com/deckgo/deckdeckgo/issues/224))

#### But why?

There are two main reasons behind the above breaking change:

1. More flexibility. With this new approach of the "reveal" effect, you could decide which elements should or should not be revealed respectively the option doesn't apply anymore on a all slide. It's now totally up to you.

2. In our upcoming editor, we had to obfuscate the "reveal" states of the elements in order to not save them in the database such information as it is not related to the deck itself but only information related to the current state of the presentation.

#### How to migrate?

1. Remove all attributes `reveal="true"` you would have set on any of your slides

2. Wrap the new above components around your elements, which should appear one at a time, as highlighted in the new [documentation](https://docs.deckdeckgo.com/edit/reveal).

# 1.0.0-beta.45-2 (2019-08-07)

### Features

- expose attributes social for studio

# 1.0.0-beta.45 (2019-08-06)

### Features

- add a new template `countdown` ([#45](https://github.com/deckgo/deckdeckgo/issues/45) and [#217](https://github.com/deckgo/deckdeckgo/issues/217))

# 1.0.0-beta.44 (2019-07-30)

### Fix

- on iOS sometimes `screen.width` is the property to use to determine the screen size, sometime it's `window.innerWidth` ([#212](https://github.com/deckgo/deckdeckgo/issues/212))

### Features

- handle Youtube short URL ([#204](https://github.com/deckgo/deckdeckgo/issues/204))

# 1.0.0-beta.43 (2019-06-19)

### Features

- watch `deckgo-youtube` property `src` to refresh video
- reflect attribute `src` in `deckgo-slide-youtube`

# 1.0.0-beta.42 (2019-06-18)

### Features

- add a new function to query the slide definitions which could be used to update the remote connection

# 1.0.0-beta.41 (2019-06-13)

### Features

- add `dev.to` to the social component

# 1.0.0-beta.40 (2019-06-24)

### Features

- update `chart` slide to support animation with reveal actions

# 1.0.0-beta.39-3 (2019-06-16)

### Fix

- post migration to Stencil One for the slide `code`

# 1.0.0-beta.39-2 (2019-06-16)

### Style

- slide `split` height inherit CSS4 variables

# 1.0.0-beta.39-1 (2019-06-14)

### Fix

fix: don't track mouse movement if deck is triggered to be blocked

# 1.0.0-beta.39 (2019-06-14)

### Features

- add a click event on the pager

### Fix

- display 0/0 not 1/0 in the pager when no slides

# 1.0.0-beta.38 (2019-06-06)

### Breaking Changes

- hide pager progression with a CSS4 variable

### Fix

- don't track slides' swipe if contextmenu is fired

### Features

- new progression text as slides count available

# 1.0.0-beta.37 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-beta.36 (2019-05-30)

### Lib

- use new package `@deckdeckgo/utils`

# 1.0.0-beta.35 (2019-05-24)

### Fix

- top and bottom padding of the slide `split`

# 1.0.0-beta.34 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/core`

# 1.0.0-beta.33 (2019-05-16)

- handles the new component `deckgo-lazy-img` and expose a new deck's method `loadBackground()` to load all the backgrounds "manually"

# 1.0.0-beta.32 (2019-04-30)

### Fix

- `slideDidLoad` event was not emitted anymore when extra deck's slots like `background` were used

# 1.0.0-beta.31 (2019-04-19)

### Breaking

- attribute `pager` of the `deckgo-deck` component has been removed. if you wish to hide the pager, use instead the new CSS4 variable `--pager-display`.

# 1.0.0-beta.30 (2019-04-18)

### Features and fixes

- some new features for the upcoming DeckDeckGo studio and a couple of fixes

# 1.0.0-beta.29 (2019-03-08)

### Templates

- add a slot `content` on the Youtube slide ([#56](https://github.com/deckgo/deckdeckgo/issues/56))

### Fix

- slide width on Android devices ([#69](https://github.com/deckgo/deckdeckgo/issues/69))

# [1.0.0-beta.28](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.27...v1.0.0-beta.28) (2019-02-26)

### Features

- on load, emit slides' titles ([8cc779e](https://github.com/deckgo/deckdeckgo/commit/8cc779e4da78354167462b9cc3e10da96efd52ee))

# [1.0.0-beta.27](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.26...v1.0.0-beta.27) (2019-02-17)

### Features

- clone background for company or branding ([#25](https://github.com/deckgo/deckdeckgo/issues/25))
- add "actions" slot ([#54](https://github.com/deckgo/deckdeckgo/issues/54))
- add youtube `toggle` method ([#51](https://github.com/deckgo/deckdeckgo/pull/51))
- improve pager and reveal transitions ([#52](https://github.com/deckgo/deckdeckgo/pull/52))

### Fix

- `clearMouseCursorTimer` is not called if fullscreen is escaped through the keyboard bug ([#53](https://github.com/deckgo/deckdeckgo/issues/53))

# [1.0.0-beta.26](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.25...v1.0.0-beta.26) (2019-02-09)

### Features

- include DeckDeckGo in any container ([#50](https://github.com/deckgo/deckdeckgo/issues/50))

# [1.0.0-beta.25](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.24...v1.0.0-beta.25) (2019-02-03)

### Features

- more padding on screen larger than 1024px ([#49](https://github.com/deckgo/deckdeckgo/issues/49))

### Fix

- RTL pager position ([#48](https://github.com/deckgo/deckdeckgo/issues/48))

# [1.0.0-beta.24](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.23...v1.0.0-beta.24) (2019-02-02)

### Features

- print one slide per page ([916768e](https://github.com/deckgo/deckdeckgo/commit/916768e96625bdda76922cf39efbb5b4573a41a0))
- don't print background slot ([e3ffd2a](https://github.com/deckgo/deckdeckgo/commit/e3ffd2aa0035ccdbcfa7f6107733bcb6896a2f03))

### Lib

- update Stencil v0.17.1 ([b90339f](https://github.com/deckgo/deckdeckgo/commit/b90339fc842ad935dd89acd1cb41e055cb21bc7d))

# [1.0.0-beta.23](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.22-1...v1.0.0-beta.23) (2019-01-20)

### Features

- RTL support ([#40](https://github.com/deckgo/deckdeckgo/issues/40))
- improve code vertical scrolling ([#41](https://github.com/deckgo/deckdeckgo/issues/41))
- expose new util method `isMobile()` ([fe1239c](https://github.com/deckgo/deckdeckgo/commit/fe1239c57503e4b669429883bd6d23c0b4a0b00f))

### Fix

- slide youtube, video might be not lazy rendered ([#42](https://github.com/deckgo/deckdeckgo/issues/42))

# [1.0.0-beta.22-1](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.22...v1.0.0-beta.22-1) (2019-01-15)

### Features

- weaker emitter type for build compatibility of the remote control pwa ([915e225](https://github.com/deckgo/deckdeckgo/commit/915e225726f1381584ee0db1d00617dd01dda8cc))

# [1.0.0-beta.22](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.21...v1.0.0-beta.22) (2019-01-15)

### Features

- notes for the editor and for the audience ([#37](https://github.com/deckgo/deckdeckgo/issues/37))
- in full screen mode, hide mouse cursor after a delay if not moved ([#39](https://github.com/deckgo/deckdeckgo/issues/39))

# [1.0.0-beta.21](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.20...v1.0.0-beta.21) (2018-12-27)

### Fix

- improve new QR Code slide flex alignment ([775ab22](https://github.com/deckgo/deckdeckgo/commit/775ab22a745975920bbe9ac4ec15ffe041ccf337))

# [1.0.0-beta.20](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.19...v1.0.0-beta.20) (2018-12-27)

### Features

- add a new template to generate easily QR code ([#36](https://github.com/deckgo/deckdeckgo/issues/36))

### Breaking Changes

- the code template as been split in a separate component. if you already use this code template, you will need to install the new `<deckgo-highlight-code/>` Web Component in your presentation ([#35](https://github.com/deckgo/deckdeckgo/issues/35))

# [1.0.0-beta.19](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.18...v1.0.0-beta.19) (2018-12-22)

### Features

- add bar charts to your presentation ([119615d](https://github.com/deckgo/deckdeckgo/commit/119615d96edcf18b458a7f998c10fa6fec2a78cb))

# [1.0.0-beta.18](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.17...v1.0.0-beta.18) (2018-12-20)

### Features

- add charts to your presentation ([#16](https://github.com/deckgo/deckdeckgo/issues/16))

# [1.0.0-beta.17](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.16...v1.0.0-beta.17) (2018-12-14)

### Fix

- emit Youtube slide loaded only after width/height of the video have been calculated ([#33](https://github.com/deckgo/deckdeckgo/issues/33))

# [1.0.0-beta.16](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.15...v1.0.0-beta.16) (2018-12-14)

### Fix

- lazy load first slides content when all slides added ([#33](https://github.com/deckgo/deckdeckgo/issues/33))

# [1.0.0-beta.15](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.14...v1.0.0-beta.15) (2018-12-13)

### Fix

- fix list items and reveal feature ([#32](https://github.com/deckgo/deckdeckgo/issues/32))

# [1.0.0-beta.14](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.13...v1.0.0-beta.14) (2018-12-13)

### Features

- don't display title on gif slide if fullscreen ([#31](https://github.com/deckgo/deckdeckgo/issues/31))

# [1.0.0-beta.13](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.12...v1.0.0-beta.13) (2018-12-13)

### Features

- lazy load images in slot="background" too ([#26](https://github.com/deckgo/deckdeckgo/issues/26))

### Fix

- lazy loaded images with parent didn't inherited the right visibility ([#30](https://github.com/deckgo/deckdeckgo/issues/30))

### Libs

- update Stencil ([e597a7b](https://github.com/deckgo/deckdeckgo/commit/e597a7b9343aa11a58a4203d1a14753af6920c75))

# [1.0.0-beta.12](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.11...v1.0.0-beta.12) (2018-12-06)

### Fix

- lazy load all slotted images not just the direct descendent ([#23](https://github.com/deckgo/deckdeckgo/issues/23))

# [1.0.0-beta.11](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.10...v1.0.0-beta.11) (2018-12-05)

### Features

- add methods to start and pause video when using the Youtube template ([#22](https://github.com/deckgo/deckdeckgo/issues/22))

# [1.0.0-beta.10](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.9...v1.0.0-beta.10) (2018-12-04)

### Features

- emit an event with descriptions of all slides when the all deck and slides are loaded ([#20](https://github.com/deckgo/deckdeckgo/issues/20))
- add a new extra slot to the deck for an extra background layer ([#21](https://github.com/deckgo/deckdeckgo/issues/21))

# [1.0.0-beta.9](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.8...v1.0.0-beta.9) (2018-12-02)

### Breaking Changes

- "author" template, the property `img-url` has been renamed to `img-src` ([#19](https://github.com/deckgo/deckdeckgo/issues/19))
- "code" template, the property `src-file` has been renamed to `src` ([#18](https://github.com/deckgo/deckdeckgo/issues/18))

### Templates and components

- add a new template and component to easily integrate Gifs ([#12](https://github.com/deckgo/deckdeckgo/issues/12))
- add a new template and component to easily integrate Youtube video ([#12](https://github.com/deckgo/deckdeckgo/issues/12))

# [1.0.0-beta.8](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.7...v1.0.0-beta.8) (2018-11-28)

### Features

- add a new event slideToChange ([#dd37c90](https://github.com/deckgo/deckdeckgo/commit/dd37c9021f2b433eb7ada5dc715c0f11910f5bcc))

# [1.0.0-beta.7](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.6...v1.0.0-beta.7) (2018-11-28)

### Breaking Changes

- event `slideNextStart` renamed to `slideNextDidChange` ([#90df088](https://github.com/deckgo/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))
- event `slidePrevStart` renamed to `slidePrevDidChange` ([#90df088](https://github.com/deckgo/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))

### Features

- add an option to not perform the inner slide animation on slideNext and slidePrev ([#cd06c76](https://github.com/deckgo/deckdeckgo/commit/cd06c76e0d7776e2c6348bd3eba58ece942255c3))
- add an option to not trigger slideStart events on slideNext and slidePrev ([#cd56d20](https://github.com/deckgo/deckdeckgo/commit/cd56d2083464007d418c190dd15ccec14737c981))

# [1.0.0-beta.6](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.5...v1.0.0-beta.6) (2018-11-27)

### Fix

- absolute is better suited than fixed if deckdeckgo is included in a not full screen page on Safari ([20b164e](https://github.com/deckgo/deckdeckgo/commit/20b164ed5b3a920b478c9be3ed67db2f6e032ffb))

# [1.0.0-beta.5](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.4...v1.0.0-beta.5) (2018-11-24)

### Features

- add features for the remote controller ([#15](https://github.com/deckgo/deckdeckgo/issues/15))

### Fix

- export missing deckdeckgo-slides.scss into the bundle ([#14](https://github.com/deckgo/deckdeckgo/issues/14))

# [1.0.0-beta.4](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.3...v1.0.0-beta.4) (2018-11-14)

### Templates

- introduce a new template to let you introduce the author of the presentation ([#9](https://github.com/deckgo/deckdeckgo/issues/9))

### Features

- highlight the code displayed with the related template using [Prismjs](https://prismjs.com) ([#4](https://github.com/deckgo/deckdeckgo/issues/4))

### Fix

- hide all lazy loaded images per default ([#10](https://github.com/deckgo/deckdeckgo/issues/10))

# [1.0.0-beta.3](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.2...v1.0.0-beta.3) (2018-11-11)

### Revert

- revert to a fixed position deck which gives better result across browsers ([af53bd5](https://github.com/deckgo/deckdeckgo/commit/af53bd5a0d49a6e3277b7d7e1bfc12f44de28b6d))

# [1.0.0-beta.2](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.1...v1.0.0-beta.2) (2018-11-11)

### Fix

- improve positioning and over scrolling on iOS ([#8](https://github.com/deckgo/deckdeckgo/issues/8))

# [1.0.0-beta.1](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.0...v1.0.0-beta.1) (2018-11-07)

### Fix

- slider was not swipeable on iOS ([#7](https://github.com/deckgo/deckdeckgo/issues/7))
