[![npm][npm-badge]][npm-badge-url]
[![license][npm-license]][npm-license-url]

[npm-badge]: https://img.shields.io/npm/v/@deckdeckgo/utils
[npm-badge-url]: https://www.npmjs.com/package/@deckdeckgo/utils
[npm-license]: https://img.shields.io/npm/l/@deckdeckgo/utils
[npm-license-url]: https://github.com/deckgo/deckdeckgo/blob/main/utils/utils/LICENSE

# DeckDeckGo - Utils

A collection of utils methods and functions developed and used across apps and components of [DeckDeckGo].

## Table of contents

-
- [Methods](#methods)
- [License](#license)

## Methods

The following functions and methods are statically exposed:

### Unify an event

To unify a mouse or touche event

```
static unifyEvent(e: any): any
```

### Debounce

Debounce a function. Per default 300ms.

```
static debounce(func: Function, timeout?: number)
```

### Is mobile?

Is the current navigator used on mobile devices? The list of devices from [Detect Mobile Browsers](http://detectmobilebrowsers.com) is use for detection purpose.

```
static isMobile(): boolean
```

### Is iOS?

Is the current device an iOS one (iPad, iPhone and iPod)?

```
static isIOS(): boolean
```

### Is full screen?

Compare the `window.innerHeight` and `screen.height` to detect full screen or not.

```
static isFullscreen(): boolean
```

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[deckdeckgo]: https://deckdeckgo.com
