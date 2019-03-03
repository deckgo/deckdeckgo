# Markdown

If you wish, you could edit your [DeckDeckGo] presentation using Markdown. This chapter has for goal to introduce you briefly on how editing your slides with Markdown differs from the HTML method.

## Table of contents

- [Introduction](#app-edit-markdown-introduction)
- [Edit](#app-edit-markdown-edit)
    - [Example](#app-edit-markdown-example)
    - [Attributes](#app-edit-markdown-attributes)
    - [Notes](#app-edit-markdown-notes)
- [Summary](#app-edit-markdown-summary)

## Introduction

When you edit your talk with Markdown, the [DeckDeckGo Webpack Markdown Plugin](https://github.com/deckgo/deckdeckgo-webpack-plugins) will convert, at bundle time, your code to HTML and will inject the results in the `index.html` file. Doing so, your [DeckDeckGo] presentation will remain SEO friendly even without server side rendering. 

## Edit

To begin to edit your talk, instead of editing the `index.html` you will instead have to edit the `index.md` file provided by the starter kit.

Furthermore than the standard Markdown tags, you will be able to use extended tags in order to specify which types of slides you would like to use. For that purpose, use the separator `---` followed by a shortened version of the template's name, like for example `--- title` for `</deckgo-slide-title>`.

The plugin also takes care of injecting the content you would provide in the right slots.

### Example

The following `<deckgo-slide-title/>` slide:

```
--- title
# My presentation title

Hello World 🚀
```

will be parsed into:

```
<deckgo-slide-title>
  <h1 slot="title">My presentation title</h1>
  <div slot="content">
    <p>Hello World 🚀</p>
  </div>
</deckgo-slide-title>
```

### Attributes

As some templates needs attributes, you will also be able to specify them in Markdown.

For example the `<deckgo-slide-gif/>` slide:

```
--- gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true"
# My title

# Hey

### It's cool gif
```

will be parsed into:

```
<deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="title">My title</h1>
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

### Notes

To add some notes to a particular slide, use the separator `***` and write down your notes afterwards.

Optionally, if you wish to display your notes in your deck, you could also use the attribute `show`.

```
--- title
# My presentation title

Hello World 🚀

***

I should not forget to think about that during my talk

...and say hello to mum
```

will be parsed into:

```
<deckgo-slide-title>
  <h1 slot="title">My presentation title</h1>
  <div slot="content">
    <p>Hello World 🚀</p>
  </div>
  <div slot="notes">
    I should not forget to think about that during my talk
    
    ...and say hello to mum
  </div>
</deckgo-slide-title>
```

## Summary

When you choose Markdown, you edit your slides in `index.md` and use standard Markdown except the extra tags `---` to declare your slides and `***` if you wish to add notes. 

[DeckDeckGo]: https://deckdeckgo.com