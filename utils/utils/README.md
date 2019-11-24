# DeckDeckGo - Types

A collection of utils methods and functions developed and used across apps and components of [DeckDeckGo].

## Table of contents

- [Getting Started](#getting-started)
- [Methods](#methods)
- [License](#license)

## Getting Started

If you would like to install and compile locally this project, proceed as following:

```
git clone https://github.com/deckgo/deckdeckgo
cd deckdeckgo/utils/utils
npm install
npm run build
```

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

[DeckDeckGo]: https://deckdeckgo.com
