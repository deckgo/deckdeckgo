# DeckDeckGo

Create a lightweight presentation using Web Components.

[DeckDeckGo] is build with [Stencil](https://stenciljs.com) and could be use in any modern framework or even without any.

## Table of contents

- [DeckDeckGo](#deckdeckgo)
	- [Table of contents](#table-of-contents)
	- [Features](#features)
		- [Starter kit extra features](#starter-kit-extra-features)
		- [Remote control](#remote-control)
  - [Getting Started](#getting-started)
  - [Installing DeckDeckGo in any projects](#installing-deckdeckgo-in-any-projects)
  - [Editing](#editing)
    - [Detailed documentation](doc/slides/slides.md)
    - [Extra components](#extra-components)
    - [Markdown](#markdown)
  - [Navigation](#navigation)
  - [Extra features](#extra-features)
  - [Lazy loading](#lazy-loading)
  - [Theming](#theming)
  - [Send me your slides](#send-me-your-slides)
  - [Backstory](#backstory)
  - [License](#license)

## Features

* ✏️ Create **without effort** your presentation using Web Components

* 📰 Use predefined **templates**

* 🌈 **Style** your presentation quickly

* 🌅 Create a **lightweight** presentation where images are **lazy** loaded

* 📱 Ship your presentation as a **Progressive Web App**

* 🎁 Free and **open source**

### Starter kit extra features

[DeckDeckGo] offers also a [starter kit](https://github.com/deckgo/deckdeckgo-starter) kit which lets you additionally:

* 🚀 Publish your presentation as a **Progressive Web App** with a **offline** support

* 🦄 Use **Ionic** components and icons to create the content or even add extra features

### Remote control

Cherry on the cake 🍒🎂 [DeckDeckGo] comes with its [Progressive Web App](https://deckdeckgo.app) that allows you to remote control your presentation 🚀

## Getting Started

[DeckDeckGo] provides a [CLI](https://github.com/deckgo/create-deckdeckgo) and a [starter kit](https://github.com/deckgo/deckdeckgo-starter).

To get started and **to create your presentation**, run the following command and follow the prompt:

```bash
npm init deckdeckgo
```

Once your presentation created, go to your new project's folder and start editing your slides and content for your talk 😉

## Installing DeckDeckGo in any projects

The [DeckDeckGo] presentation tool could also be use without any tooling. If you wish import the component in your web project, have a look to the [installation documentation](doc/installation/installation.md) to learn how to install and include it.

## Editing

[DeckDeckGo] is a deck of slides where each slide has its own layout and behaviour. Their content could be edited and structured using the provided `slots` and other attributes.

The deck should be declared using the tag `<deckgo-deck/>` and each slide should be added to its children.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">The first slide</h1>
    <p slot="content">
      Hello World 🚀
    </p>
  </deckgo-slide-title>
  
  <deckgo-slide-content>
      <h1 slot="title">The second slide</h1>
  </deckgo-slide-content>
</deckgo-deck>
```

The 👉 [extended documentation](doc/slides/slides.md) 👈 displays all templates and options available for each slides.

### Extra components

[DeckDeckGo] also offers a couple of components which could be used in almost every templates, these are documented in a separate [chapter](doc/components/components.md) of the documentation. 

### Markdown

Per default, [DeckDeckGo]'s presentations are edited using HTML and are Web Components, that's why the documentation and examples are provided in HTML. But if you are using the [starter kit](https://github.com/deckgo/deckdeckgo-starter) and are editing your talk with Markdown, you could have a look to the [Markdown documentation](doc/markdown/markdown.md) to get some examples.

## Navigation

Furthermore than the default swiping, the [DeckDeckGo] deck expose some asynchronous methods in case you would like to add navigation features to your presentation.

These features are described in the separate [documentation](doc/features/navigation.md) about navigation. 

## Extra features

Finally [DeckDeckGo] offers extra features, as for example a print feature or a full screen toggler, which could be added to your presentation too.

These features are described in the separate [documentation](doc/features/extra.md) about extra features.

## Lazy loading

In order to lazy load the images of your presentation, provide their url using the attribute `data-src` instead of `src`. [DeckDeckGo] will then take care of loading them only when needed.

```
<img data-src="https://deckdeckgo.com/assets/img/deckdeckgo.png"/>
```

## Theming

[DeckDeckGo] offers various theming options which could be set using CSS variables and which are described in their respective slides' templates [documentation](doc/slides/slides.md).

*Note: If you would miss or need further theming options, don't hesitate to open an issue and/or submit a PR, it would be my pleasure to add more theming flexibility and options*

### RTL Support

[DeckDeckGo] offers full LTR and RTL support. The deck inherits its parent text direction.

Commonly, if you wish to use RTL for your all page respectively presentation, you could set the attribute `dir` of the root `html` tag to `rtl`.    

```
<!DOCTYPE html>
<html dir="rtl">
<body>
  <deckgo-deck>
  </deckgo-deck>
</body>
</html>  
```

## Send me your slides

If you would publish online a presentation or talk you would have built with [DeckDeckGo], reach me out, I would be super duper happy to add it to the [list of talks and presentations](doc/talks/talks.md) ❤️

## Backstory

I had the opportunity to talk about Web Components and Ionic. While I was developing my presentation it came to my mind that I was not really following what I was about to present, that's why I wrapped up together [DeckDeckGo], this new tool to create lightweight presentation using HTML, Web Components and Ionic.

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com)

[DeckDeckGo]: https://deckdeckgo.com
