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
