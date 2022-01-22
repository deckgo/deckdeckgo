[![npm][npm-badge]][npm-badge-url]
[![license][npm-license]][npm-license-url]

[npm-badge]: https://img.shields.io/npm/v/@deckdeckgo/monaco-editor
[npm-badge-url]: https://www.npmjs.com/package/@deckdeckgo/monaco-editor
[npm-license]: https://img.shields.io/npm/l/@deckdeckgo/monaco-editor
[npm-license-url]: https://github.com/deckgo/deckdeckgo/blob/main/webcomponenents/monaco-editor/LICENSE

# DeckDeckGo - Monaco Editor

A web component to easily embed the [Monaco Editor](https://microsoft.github.io/monaco-editor/).

## Installation

```
npm i @deckdeckgo/monaco-editor
```

## Usage

1. Import the component in your application, for example with an `import` script.

```
import @deckdeckgo/monaco-editor
```

2. Copy the pre-compiled workers to your `public` folder or a sub-folder.

```
copy: [
    {src: `${__dirname}/node_modules/@deckdeckgo/monaco-editor/workers/`, dest: `${__dirname}/public`}
]
```

3. Configure where the worker scripts are located i.e. add following script in your application (update the path `./` if you have copied the scripts in sub-folders).

```
self.MonacoEnvironment = {
    getWorkerUrl: function (_moduleId, label) {
      if (label === 'json') {
        return './json.worker.js';
      }
      if (label === 'css' || label === 'scss' || label === 'less') {
        return './css.worker.js';
      }
      if (label === 'html' || label === 'handlebars' || label === 'razor') {
        return './html.worker.js';
      }
      if (label === 'typescript' || label === 'javascript') {
        return './ts.worker.js';
      }
      return './editor.worker.js';
    }
};
```

4. Use the component

```
<deckgo-monaco-editor></deckgo-monaco-editor>
```

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[deckdeckgo]: https://deckdeckgo.com
