<a name="1.0.0-beta.43"></a>
# 1.0.0-beta.43 (2019-06-19)

### Features

* watch `deckgo-youtube` property `src` to refresh video
* reflect attribute `src` in `deckgo-slide-youtube`

<a name="1.0.0-beta.42"></a>
# 1.0.0-beta.42 (2019-06-18)

### Features

* add a new function to query the slide definitions which could be used to update the remote connection

<a name="1.0.0-beta.41"></a>
# 1.0.0-beta.41 (2019-06-13)

### Features

* add `dev.to` to the social component

<a name="1.0.0-beta.40"></a>
# 1.0.0-beta.40 (2019-06-24)

### Features

* update `chart` slide to support animation with reveal actions

<a name="1.0.0-beta.39-3"></a>
# 1.0.0-beta.39-3 (2019-06-16)

### Fix

* post migration to Stencil One for the slide `code`

<a name="1.0.0-beta.39-2"></a>
# 1.0.0-beta.39-2 (2019-06-16)

### Style

* slide `split` height inherit CSS4 variables

<a name="1.0.0-beta.39-1"></a>
# 1.0.0-beta.39-1 (2019-06-14)

### Fix

fix: don't track mouse movement if deck is triggered to be blocked

<a name="1.0.0-beta.39"></a>
# 1.0.0-beta.39 (2019-06-14)

### Features

* add a click event on the pager

### Fix

* display 0/0 not 1/0 in the pager when no slides

<a name="1.0.0-beta.38"></a>
# 1.0.0-beta.38 (2019-06-06)

### Breaking changes

* hide pager progression with a CSS4 variable

### Fix

* don't track slides' swipe if contextmenu is fired

### Features

* new progression text as slides count available

<a name="1.0.0-beta.37"></a>
# 1.0.0-beta.37 (2019-06-04)

### Libs

* upgrade to Stencil One

<a name="1.0.0-beta.36"></a>
# 1.0.0-beta.36 (2019-05-30)

### Lib

* use new package `@deckdeckgo/utils`

<a name="1.0.0-beta.35"></a>
# 1.0.0-beta.35 (2019-05-24)

### Fix

* top and bottom padding of the slide `split`

<a name="1.0.0-beta.34"></a>
# 1.0.0-beta.34 (2019-05-24)

### Breaking

* move to the org scoped package `@deckdeckgo/core`

<a name="1.0.0-beta.33"></a>
# 1.0.0-beta.33 (2019-05-16)

* handles the new component `deckgo-lazy-img` and expose a new deck's method `loadBackground()` to load all the backgrounds "manually"

<a name="1.0.0-beta.32"></a>
# 1.0.0-beta.32 (2019-04-30)

### Fix

* `slideDidLoad` event was not emitted anymore when extra deck's slots like `background` were used

<a name="1.0.0-beta.31"></a>
# 1.0.0-beta.31 (2019-04-19)

### Breaking

* attribute `pager` of the `deckgo-deck` component has been removed. if you wish to hide the pager, use instead the new CSS4 variable `--pager-display`.

<a name="1.0.0-beta.30"></a>
# 1.0.0-beta.30 (2019-04-18)

### Features and fixes

* some new features for the upcoming DeckDeckGo studio and a couple of fixes

<a name="1.0.0-beta.29"></a>
# 1.0.0-beta.29 (2019-03-08)

### Templates

* add a slot `content` on the Youtube slide ([#56](https://github.com/deckgo/deckdeckgo/issues/56))

### Fix

* slide width on Android devices ([#69](https://github.com/deckgo/deckdeckgo/issues/69))

<a name="1.0.0-beta.28"></a>
# [1.0.0-beta.28](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.27...v1.0.0-beta.28) (2019-02-26)

### Features

* on load, emit slides' titles ([8cc779e](https://github.com/deckgo/deckdeckgo/commit/8cc779e4da78354167462b9cc3e10da96efd52ee))

<a name="1.0.0-beta.27"></a>
# [1.0.0-beta.27](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.26...v1.0.0-beta.27) (2019-02-17)

### Features

* clone background for company or branding ([#25](https://github.com/deckgo/deckdeckgo/issues/25))
* add "actions" slot ([#54](https://github.com/deckgo/deckdeckgo/issues/54))
* add youtube `toggle` method ([#51](https://github.com/deckgo/deckdeckgo/pull/51))
* improve pager and reveal transitions ([#52](https://github.com/deckgo/deckdeckgo/pull/52))

### Fix

* `clearMouseCursorTimer` is not called if fullscreen is escaped through the keyboard bug ([#53](https://github.com/deckgo/deckdeckgo/issues/53))

<a name="1.0.0-beta.26"></a>
# [1.0.0-beta.26](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.25...v1.0.0-beta.26) (2019-02-09)

### Features

* include DeckDeckGo in any container ([#50](https://github.com/deckgo/deckdeckgo/issues/50))

<a name="1.0.0-beta.25"></a>
# [1.0.0-beta.25](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.24...v1.0.0-beta.25) (2019-02-03)

### Features

* more padding on screen larger than 1024px ([#49](https://github.com/deckgo/deckdeckgo/issues/49))

### Fix

* RTL pager position ([#48](https://github.com/deckgo/deckdeckgo/issues/48))

<a name="1.0.0-beta.24"></a>
# [1.0.0-beta.24](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.23...v1.0.0-beta.24) (2019-02-02)

### Features

* print one slide per page ([916768e](https://github.com/deckgo/deckdeckgo/commit/916768e96625bdda76922cf39efbb5b4573a41a0))
* don't print background slot ([e3ffd2a](https://github.com/deckgo/deckdeckgo/commit/e3ffd2aa0035ccdbcfa7f6107733bcb6896a2f03))

### Lib

* update Stencil v0.17.1 ([b90339f](https://github.com/deckgo/deckdeckgo/commit/b90339fc842ad935dd89acd1cb41e055cb21bc7d))

<a name="1.0.0-beta.23"></a>
# [1.0.0-beta.23](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.22-1...v1.0.0-beta.23) (2019-01-20)

### Features

* RTL support ([#40](https://github.com/deckgo/deckdeckgo/issues/40))
* improve code vertical scrolling ([#41](https://github.com/deckgo/deckdeckgo/issues/41))
* expose new util method `isMobile()` ([fe1239c](https://github.com/deckgo/deckdeckgo/commit/fe1239c57503e4b669429883bd6d23c0b4a0b00f))

### Fix

* slide youtube, video might be not lazy rendered ([#42](https://github.com/deckgo/deckdeckgo/issues/42))

<a name="1.0.0-beta.22-1"></a>
# [1.0.0-beta.22-1](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.22...v1.0.0-beta.22-1) (2019-01-15)

### Features

* weaker emitter type for build compatibility of the remote control pwa ([915e225](https://github.com/deckgo/deckdeckgo/commit/915e225726f1381584ee0db1d00617dd01dda8cc))

<a name="1.0.0-beta.22"></a>
# [1.0.0-beta.22](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.21...v1.0.0-beta.22) (2019-01-15)

### Features

* notes for the editor and for the audience ([#37](https://github.com/deckgo/deckdeckgo/issues/37))
* in full screen mode, hide mouse cursor after a delay if not moved ([#39](https://github.com/deckgo/deckdeckgo/issues/39))

<a name="1.0.0-beta.21"></a>
# [1.0.0-beta.21](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.20...v1.0.0-beta.21) (2018-12-27)

### Fix

* improve new QR Code slide flex alignment ([775ab22](https://github.com/deckgo/deckdeckgo/commit/775ab22a745975920bbe9ac4ec15ffe041ccf337))

<a name="1.0.0-beta.20"></a>
# [1.0.0-beta.20](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.19...v1.0.0-beta.20) (2018-12-27)

### Features

* add a new template to generate easily QR code ([#36](https://github.com/deckgo/deckdeckgo/issues/36))

### Breaking Changes

* the code template as been split in a separate component. if you already use this code template, you will need to install the new `<deckgo-highlight-code/>` Web Component in your presentation ([#35](https://github.com/deckgo/deckdeckgo/issues/35))

<a name="1.0.0-beta.19"></a>
# [1.0.0-beta.19](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.18...v1.0.0-beta.19) (2018-12-22)

### Features

* add bar charts to your presentation ([119615d](https://github.com/deckgo/deckdeckgo/commit/119615d96edcf18b458a7f998c10fa6fec2a78cb))

<a name="1.0.0-beta.18"></a>
# [1.0.0-beta.18](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.17...v1.0.0-beta.18) (2018-12-20)

### Features

* add charts to your presentation ([#16](https://github.com/deckgo/deckdeckgo/issues/16))

<a name="1.0.0-beta.17"></a>
# [1.0.0-beta.17](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.16...v1.0.0-beta.17) (2018-12-14)

### Fix

* emit Youtube slide loaded only after width/height of the video have been calculated ([#33](https://github.com/deckgo/deckdeckgo/issues/33))

<a name="1.0.0-beta.16"></a>
# [1.0.0-beta.16](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.15...v1.0.0-beta.16) (2018-12-14)

### Fix

* lazy load first slides content when all slides added ([#33](https://github.com/deckgo/deckdeckgo/issues/33))

<a name="1.0.0-beta.15"></a>
# [1.0.0-beta.15](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.14...v1.0.0-beta.15) (2018-12-13)

### Fix

* fix list items and reveal feature ([#32](https://github.com/deckgo/deckdeckgo/issues/32))

<a name="1.0.0-beta.14"></a>
# [1.0.0-beta.14](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.13...v1.0.0-beta.14) (2018-12-13)

### Features

* don't display title on gif slide if fullscreen ([#31](https://github.com/deckgo/deckdeckgo/issues/31))

<a name="1.0.0-beta.13"></a>
# [1.0.0-beta.13](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.12...v1.0.0-beta.13) (2018-12-13)

### Features

* lazy load images in slot="background" too ([#26](https://github.com/deckgo/deckdeckgo/issues/26))

### Fix

* lazy loaded images with parent didn't inherited the right visibility ([#30](https://github.com/deckgo/deckdeckgo/issues/30))

### Libs

* update Stencil ([e597a7b](https://github.com/deckgo/deckdeckgo/commit/e597a7b9343aa11a58a4203d1a14753af6920c75))

<a name="1.0.0-beta.12"></a>
# [1.0.0-beta.12](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.11...v1.0.0-beta.12) (2018-12-06)

### Fix

* lazy load all slotted images not just the direct descendent ([#23](https://github.com/deckgo/deckdeckgo/issues/23))

<a name="1.0.0-beta.11"></a>
# [1.0.0-beta.11](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.10...v1.0.0-beta.11) (2018-12-05)

### Features

* add methods to start and pause video when using the Youtube template ([#22](https://github.com/deckgo/deckdeckgo/issues/22))

<a name="1.0.0-beta.10"></a>
# [1.0.0-beta.10](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.9...v1.0.0-beta.10) (2018-12-04)

### Features

* emit an event with descriptions of all slides when the all deck and slides are loaded ([#20](https://github.com/deckgo/deckdeckgo/issues/20))
* add a new extra slot to the deck for an extra background layer ([#21](https://github.com/deckgo/deckdeckgo/issues/21))

<a name="1.0.0-beta.9"></a>
# [1.0.0-beta.9](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.8...v1.0.0-beta.9) (2018-12-02)

### Breaking Changes

* "author" template, the property `img-url` has been renamed to `img-src` ([#19](https://github.com/deckgo/deckdeckgo/issues/19))
* "code" template, the property `src-file` has been renamed to `src` ([#18](https://github.com/deckgo/deckdeckgo/issues/18))

### Templates and components

* add a new template and component to easily integrate Gifs ([#12](https://github.com/deckgo/deckdeckgo/issues/12))
* add a new template and component to easily integrate Youtube video ([#12](https://github.com/deckgo/deckdeckgo/issues/12))

<a name="1.0.0-beta.8"></a>
# [1.0.0-beta.8](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.7...v1.0.0-beta.8) (2018-11-28)

### Features

* add a new event slideToChange ([#dd37c90](https://github.com/deckgo/deckdeckgo/commit/dd37c9021f2b433eb7ada5dc715c0f11910f5bcc))

<a name="1.0.0-beta.7"></a>
# [1.0.0-beta.7](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.6...v1.0.0-beta.7) (2018-11-28)

### Breaking changes

* event `slideNextStart` renamed to `slideNextDidChange` ([#90df088](https://github.com/deckgo/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))
* event `slidePrevStart` renamed to `slidePrevDidChange` ([#90df088](https://github.com/deckgo/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))

### Features

* add an option to not perform the inner slide animation on slideNext and slidePrev ([#cd06c76](https://github.com/deckgo/deckdeckgo/commit/cd06c76e0d7776e2c6348bd3eba58ece942255c3))
* add an option to not trigger slideStart events on slideNext and slidePrev ([#cd56d20](https://github.com/deckgo/deckdeckgo/commit/cd56d2083464007d418c190dd15ccec14737c981))

<a name="1.0.0-beta.6"></a>
# [1.0.0-beta.6](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.5...v1.0.0-beta.6) (2018-11-27)

### Fix

* absolute is better suited than fixed if deckdeckgo is included in a not full screen page on Safari ([20b164e](https://github.com/deckgo/deckdeckgo/commit/20b164ed5b3a920b478c9be3ed67db2f6e032ffb))

<a name="1.0.0-beta.5"></a>
# [1.0.0-beta.5](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.4...v1.0.0-beta.5) (2018-11-24)

### Features

* add features for the remote controller ([#15](https://github.com/deckgo/deckdeckgo/issues/15))

### Fix

* export missing deckdeckgo-slides.scss into the bundle ([#14](https://github.com/deckgo/deckdeckgo/issues/14))

<a name="1.0.0-beta.4"></a>
# [1.0.0-beta.4](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.3...v1.0.0-beta.4) (2018-11-14)

### Templates

* introduce a new template to let you introduce the author of the presentation ([#9](https://github.com/deckgo/deckdeckgo/issues/9))

### Features

* highlight the code displayed with the related template using [Prismjs](https://prismjs.com) ([#4](https://github.com/deckgo/deckdeckgo/issues/4))

### Fix

* hide all lazy loaded images per default ([#10](https://github.com/deckgo/deckdeckgo/issues/10))

<a name="1.0.0-beta.3"></a>
# [1.0.0-beta.3](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.2...v1.0.0-beta.3) (2018-11-11)

### Revert

* revert to a fixed position deck which gives better result across browsers ([af53bd5](https://github.com/deckgo/deckdeckgo/commit/af53bd5a0d49a6e3277b7d7e1bfc12f44de28b6d))

<a name="1.0.0-beta.2"></a>
# [1.0.0-beta.2](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.1...v1.0.0-beta.2) (2018-11-11)

### Fix

* improve positioning and over scrolling on iOS ([#8](https://github.com/deckgo/deckdeckgo/issues/8))

<a name="1.0.0-beta.1"></a>
# [1.0.0-beta.1](https://github.com/deckgo/deckdeckgo/compare/v1.0.0-beta.0...v1.0.0-beta.1) (2018-11-07)

### Fix

* slider was not swipeable on iOS ([#7](https://github.com/deckgo/deckdeckgo/issues/7))
