# DeckDeckGo - Markdown

This documentation has for goal to introduce you briefly on how is it working out if you are using the [starter kit](https://github.com/deckgo/deckdeckgo-starter) and are editing your talk with Markdown.

## Table of contents

- [Introduction](#introduction)
- [Editing](#editing)
    - [Example](#example)
    - [Attributes](#attributes)

## Introduction

It's pretty simple, the goal of the editing with Markdown is to offer the same abilities as if you would have while editing your talk with HTML.

When you edit your talk with Markdown, furthermore than the set of common supported tags, the [DeckDeckGo Webpack Markdown Plugin](https://github.com/deckgo/deckdeckgo-webpack-markdown-plugin) extend these by adding the support of the different [DeckDeckGo] slides as documented in the 👉 [extended documentation](doc/slides/slides.md) 👈.

Your Markdown code will, through the help of the plugin, be compiled and injected in the `index.html` distribution file at build time. Doing so, your [DeckDeckGo] presentation will remain SEO friendly even without server side rendering. 

## Editing

As describe above, furthermore than the Markdown tags, you could use extended tags in order to specify which types of slides you would like to use. To do so, use the separator `---` followed by a shortened version of the template's name, like for example `--- title` for `</deckgo-slide-title>`.

Furthermore the plugin takes care of injecting the content you would provide in the right slots.

### Example

The following `</deckgo-slide-title>` slide:

```
--- title
# My presentation title

Hello World 🚀
```

would be parsed into:

```
<deckgo-slide-title>
  <h1 slot="title">My presentation title</h1>
  <p slot="content">
    Hello World 🚀
  </p>
</deckgo-slide-title>
```

### Attributes

As some templates needs attributes, you would also be able to specify them in Markdown.

For example the `</deckgo-slide-gif>` slide:

```
--- gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true"
# My title

# Hey

### It's cool gif
```

would be parsed into:

```
<deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="title">My title</h1>
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

[DeckDeckGo]: https://deckdeckgo.com