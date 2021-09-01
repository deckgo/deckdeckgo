# 2.2.0 (2021-09-01)

### Features

- do not generate new random colors on redraw

### Build

- bump dependencies

# 2.1.0 (2021-05-29)

### Build

- output custom elements as a single bundle (`dist-custom-elements-bundle`)
- bump dependencies

# 2.0.1 (2020-10-23)

### Fix

- es5 has been dropped with v2, update `unpkg` reference

# 2.0.0 (2020-09-03)

### Breaking Changes

- IE11, Edge 16-18 and Safari 10 not supported

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

# 1.0.0-rc.6-1 (2020-01-16)

- add option `yAxisMin` to set a minimal value for the y Axis. useful in case the series of data could contains only zeros.

# 1.0.0-rc.6 (2020-01-16)

- add option to custom load (fetch) the data

# 1.0.0-rc.5 (2019-11-30)

- update margin and positioning of the charts

# 1.0.0-rc.4 (2019-11-30)

### Features

- add new property `data` and `yAxis` to the bar chart
- add new method `updateCurrentBar()` to the bar chart
- add new CSS variables for the fonts to the charts

Note: some **major** internal modifications where developed to the bar chart in order to integrate it to the new `poll` template

# 1.0.0-rc.3-1 (2019-10-20)

### Fix

- custom labels for x-axis compatible with ES5

# 1.0.0-rc.3 (2019-10-19)

### Breaking changes

- indexing of the style of `line bar` chart has been moved from index `0` to begin with `1`
- property `range` replaced by CSS4 variables generated dynamically

### Features

- add a new CSS4 variable `--deckgo-chart-stroke` in case you would like to use a default styling color for all the graphs' lines
- update most recent libs

# 1.0.0-rc.2 (2019-10-09)

### Features

- when using animation bar, allow custom labels for x-axis ([#340](https://github.com/deckgo/deckdeckgo/issues/340))

# 1.0.0-rc.1 (2019-08-30)

### Libs

- update to most recent dependencies

### Note about v1.0.0-rc.1

The first users began to test, and to create content in, our web open source editor for presentations (`studio`)

# 1.0.0-alpha.8 (2019-06-24)

### Features

- animated charts: it is now possible to display several data in the same graphs. these could be displayed one after the others with a nice transition between these

### Breaking

- the styling (fill color, etc.) of the `line` and `bar` chart has been modified
- per default, the labels of the axis of the `line` and `bar` will be displayed

Referer to the updated [documentation](https://docs.deckdeckgo.com) for more information.

# 1.0.0-alpha.7 (2019-06-04)

### Libs

- upgrade to Stencil One

# 1.0.0-alpha.6 (2019-05-24)

### Breaking

- move to the org scoped package `@deckdeckgo/charts`

# [1.0.0-alpha.5](https://github.com/fluster/deckdeckgo-charts/compare/v1.0.0-alpha.4...v1.0.0-alpha.5) (2019-02-07)

### Features

- feat: redraw chart if width, height or src change ([01aa026](https://github.com/deckgo/deckdeckgo-charts/commit/01aa026f0ab746684abf1e9e83b975ea15eaaef1))
- feat: add text variable to bar and line charts ([54cbbb2](https://github.com/deckgo/deckdeckgo-charts/commit/54cbbb2a11a4873e9462b48dd41d0fb6985d5ef5))

# [1.0.0-alpha.4](https://github.com/fluster/deckdeckgo-charts/compare/v1.0.0-alpha.3...v1.0.0-alpha.4) (2019-02-02)

### Lib

- update Stencil and d3js ([530c090](https://github.com/deckgo/deckdeckgo-charts/commit/530c090eb85862576758c31886c9d38d1c2c98df))
