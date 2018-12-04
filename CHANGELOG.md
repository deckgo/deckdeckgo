<a name="1.0.0-beta.10"></a>
# [1.0.0-beta.10](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.9...v1.0.0-beta.10) (2018-11-04)

### Features

* emit an event with descriptions of all slides when the all deck and slides are loaded ([#20](https://github.com/fluster/deckdeckgo/issues/20))
* add a new extra slot to the deck for an extra background layer ([#21](https://github.com/fluster/deckdeckgo/issues/21))

<a name="1.0.0-beta.9"></a>
# [1.0.0-beta.9](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.8...v1.0.0-beta.9) (2018-11-02)

### Breaking Changes

* "author" template, the property `img-url` has been renamed to `img-src` ([#19](https://github.com/fluster/deckdeckgo/issues/19))
* "code" template, the property `src-file` has been renamed to `src` ([#18](https://github.com/fluster/deckdeckgo/issues/18))

### Templates and components

* add a new template and component to easily integrate Gifs ([#12](https://github.com/fluster/deckdeckgo/issues/12))
* add a new template and component to easily integrate Youtube video ([#12](https://github.com/fluster/deckdeckgo/issues/12))

<a name="1.0.0-beta.8"></a>
# [1.0.0-beta.8](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.7...v1.0.0-beta.8) (2018-11-28)

### Features

* add a new event slideToChange ([#dd37c90](https://github.com/fluster/deckdeckgo/commit/dd37c9021f2b433eb7ada5dc715c0f11910f5bcc))

<a name="1.0.0-beta.7"></a>
# [1.0.0-beta.7](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.6...v1.0.0-beta.7) (2018-11-28)

### Breaking changes

* event `slideNextStart` renamed to `slideNextDidChange` ([#90df088](https://github.com/fluster/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))
* event `slidePrevStart` renamed to `slidePrevDidChange` ([#90df088](https://github.com/fluster/deckdeckgo/commit/90df0889345c15a3969a92cec00e90e4a3e79649))

### Features

* add an option to not perform the inner slide animation on slideNext and slidePrev ([#cd06c76](https://github.com/fluster/deckdeckgo/commit/cd06c76e0d7776e2c6348bd3eba58ece942255c3))
* add an option to not trigger slideStart events on slideNext and slidePrev ([#cd56d20](https://github.com/fluster/deckdeckgo/commit/cd56d2083464007d418c190dd15ccec14737c981))

<a name="1.0.0-beta.6"></a>
# [1.0.0-beta.6](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.5...v1.0.0-beta.6) (2018-11-27)

### Fix

* absolute is better suited than fixed if deckdeckgo is included in a not full screen page on Safari ([20b164e](https://github.com/fluster/deckdeckgo/commit/20b164ed5b3a920b478c9be3ed67db2f6e032ffb))

<a name="1.0.0-beta.5"></a>
# [1.0.0-beta.5](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.4...v1.0.0-beta.5) (2018-11-24)

### Features

* add features for the remote controller ([#15](https://github.com/fluster/deckdeckgo/issues/15))

### Fix

* export missing deckdeckgo-slides.scss into the bundle ([#14](https://github.com/fluster/deckdeckgo/issues/14))

<a name="1.0.0-beta.4"></a>
# [1.0.0-beta.4](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.3...v1.0.0-beta.4) (2018-11-14)

### Templates

* introduce a new template to let you introduce the author of the presentation ([#9](https://github.com/fluster/deckdeckgo/issues/9))

### Features

* highlight the code displayed with the related template using [Prismjs](https://prismjs.com) ([#4](https://github.com/fluster/deckdeckgo/issues/4))

### Fix

* hide all lazy loaded images per default ([#10](https://github.com/fluster/deckdeckgo/issues/10))

<a name="1.0.0-beta.3"></a>
# [1.0.0-beta.3](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.2...v1.0.0-beta.3) (2018-11-11)

### Revert

* revert to a fixed position deck which gives better result across browsers ([af53bd5](https://github.com/fluster/deckdeckgo/commit/af53bd5a0d49a6e3277b7d7e1bfc12f44de28b6d))

<a name="1.0.0-beta.2"></a>
# [1.0.0-beta.2](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.1...v1.0.0-beta.2) (2018-11-11)

### Fix

* improve positioning and over scrolling on iOS ([#8](https://github.com/fluster/deckdeckgo/issues/8))

<a name="1.0.0-beta.1"></a>
# [1.0.0-beta.1](https://github.com/fluster/deckdeckgo/compare/v1.0.0-beta.0...v1.0.0-beta.1) (2018-11-07)

### Fix

* slider was not swipeable on iOS ([#7](https://github.com/fluster/deckdeckgo/issues/7))
