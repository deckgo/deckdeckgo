[![npm][npm-badge]][npm-badge-url]
[![license][npm-license]][npm-license-url]

[npm-badge]: https://img.shields.io/npm/v/deckdeckgo-webpack-plugins
[npm-badge-url]: https://www.npmjs.com/package/deckdeckgo-webpack-plugins
[npm-license]: https://img.shields.io/npm/l/deckdeckgo-webpack-plugins
[npm-license-url]: https://github.com/deckgo/deckdeckgo/blob/main/webpack/LICENSE

# DeckDeckGo - Webpack Plugins

This project is a collection of custom Webpack plugins used to build a [DeckDeckGo] presentation.

## Table of contents

- [Webpack Markdown Plugin](#webpack-markdown-plugin)
- [Webpack Info Plugin](#webpack-info-plugin)
- [Webpack Notes Plugin](#webpack-notes-plugin)
- [License](#license)

## Webpack Markdown Plugin

This Webpack plugin has for goal to convert your presentation written in Markdown to HTML including the [DeckDeckGo] tags and markers.

For the Markdown parsing itself, the [remarkable](https://github.com/jonschlinkert/remarkable) parser is used under the hood. In the particular case of this plugin, it will be extended in order to enhance it with the following abilities:

1. Parse all images with **lazy loading** support
2. Allow the parsing of [Ionic](https://ionicframework.com) and [DeckDeckGo] html tags

## Webpack Info Plugin

This Webpack plugin is a simple plugin which has for goal to display some information after the build has completed.

## Webpack Notes Plugin

This Webpack plugin has for goal to remove your notes from your presentation, notably useful when you will run a production build before publishing your presentation online.

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[deckdeckgo]: https://deckdeckgo.com
