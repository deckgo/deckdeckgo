<a name="1.0.0-alpha.12"></a>
# [1.0.0-alpha.12](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.11...v1.0.0-alpha.12) (2019-03-03)

### Features

* style: add a variable for the display of the shadowed code element ([e00a3ea](https://github.com/deckgo/deckdeckgo/commit/e00a3ea170330de867cfedcdd0390c56988ae2f7))

<a name="1.0.0-alpha.11"></a>
# [1.0.0-alpha.11](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.10...v1.0.0-alpha.11) (2019-02-11)

### Breaking changes

* refactor CSS4 margin variable to give access to the all margin element not just margin-bottom ((ec4d260)[https://github.com/deckgo/deckdeckgo-highlight-code/commit/ec4d2602d3fa10e8e1ddf0ee20790d1fd6646720])

<a name="1.0.0-alpha.10"></a>
# [1.0.0-alpha.10](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.9...v1.0.0-alpha.10) (2019-02-01)

### Features

* add a method to load or reload the component ([ab11a8c](https://github.com/deckgo/deckdeckgo-highlight-code/commit/ab11a8c3be46bb7b6f3cdee6d9c7b0119baea595))
* loading of component with src and particular language ([ed88c8e](https://github.com/deckgo/deckdeckgo-highlight-code/commit/ed88c8e92aceebeb7d673819a8bed616d8ed8022))
* prevent error in case language is set as empty ([3790fc9](https://github.com/deckgo/deckdeckgo-highlight-code/commit/3790fc94da019047df7792628478f7a7d0c390d5))

<a name="1.0.0-alpha.9"></a>
# [1.0.0-alpha.9](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.8...v1.0.0-alpha.9) (2019-02-01)

### Fix and rollback

* if not shadowed the component was not compatible if used in other shadowed components, therefore rollback to a shadowed component ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

### Features

* add height new CSS4 variables to style the highlighted code ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

<a name="1.0.0-alpha.8"></a>
# [1.0.0-alpha.8](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.8...v1.0.0-alpha.8) (2019-02-01)

### Fix

* when the component was used in shadowed component, sometimes the slot content was not loaded ([6df0d0e](https://github.com/deckgo/deckdeckgo-highlight-code/commit/6df0d0e6802afeba2277caa90b1b0e4ddcc39fed))

<a name="1.0.0-alpha.7"></a>
# [1.0.0-alpha.7](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.6...v1.0.0-alpha.7) (2019-02-01)

### Features

* this Web Component isn't shadowed anymore to give you full access to its style ([#5](https://github.com/deckgo/deckdeckgo-highlight-code/issues/5))

### Fix

* synchronize load languages across multiple components ([44fac83](https://github.com/deckgo/deckdeckgo-highlight-code/commit/44fac8378d6ea0d88ccb67175e3dcca2f0b9bb52))

<a name="1.0.0-alpha.6"></a>
# [1.0.0-alpha.6](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.5...v1.0.0-alpha.6) (2019-01-31)

### Refactor

* export anchor interface declaration ([680a2d3](https://github.com/deckgo/deckdeckgo-highlight-code/commit/680a2d30746a9cb86c617433b46ccb5c30894491))

<a name="1.0.0-alpha.5"></a>
# [1.0.0-alpha.5](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.4...v1.0.0-alpha.5) (2019-01-23)

### Bugs

* highlight code lines: first element might be a text ([#4](https://github.com/deckgo/deckdeckgo-highlight-code/issues/4))

<a name="1.0.0-alpha.4"></a>
# [1.0.0-alpha.4](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.3...v1.0.0-alpha.4) (2019-01-23)

### Bugs

* slot code: unescape characters ([#2](https://github.com/deckgo/deckdeckgo-highlight-code/issues/2))
* CSS text-align per default left ([#3](https://github.com/deckgo/deckdeckgo-highlight-code/issues/3))

<a name="1.0.0-alpha.3"></a>
# [1.0.0-alpha.3](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.2...v1.0.0-alpha.3) (2019-01-20)

### Features

* highlight code lines ([#1](https://github.com/deckgo/deckdeckgo-highlight-code/issues/1))

<a name="1.0.0-alpha.2"></a>
# [1.0.0-alpha.2](https://github.com/deckgo/deckdeckgo-highlight-code/compare/v1.0.0-alpha.1...v1.0.0-alpha.2) (2019-01-20)

### Breaking changes

* this Web Component doesn't provide a scroll assist method anymore but rather provide a method `findNextAnchor()` which would provide you the information about the next anchor position to scroll to
