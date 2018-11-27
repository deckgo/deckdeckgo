# DeckDeckGo

Create a lightweight presentation using HTML and Web Components.

[DeckDeckGo] is build with [Stencil](https://stenciljs.com) and could be use in any modern framework or even without any.

## Table of contents

- [DeckDeckGo](#deckdeckgo)
	- [Table of contents](#table-of-contents)
	- [Features](#features)
		- [Starter kit extra features](#starter-kit-extra-features)
		- [Remote control](#remote-control)
  - [Getting Started](#getting-started)
    - [Installing DeckDeckGo in any projects](#installing-deckdeckgo-in-any-projects)
      - [Using DeckDeckGo from a CDN](#using-deckdeckgo-from-a-cdn)
      - [Install DeckDeckGo from npm](#install-deckdeckgo-from-npm)
      - [Framework integration](#framework-integration)
  - [Editing](#editing)
    - [Detailed documentation](doc/slides/slides.md)
  - [Navigation](#navigation)
  - [Extra features](#extra-features)
  - [Lazy loading](#lazy-loading)
  - [Theming](#theming)
  - [Talks](#talks)
    - [Send me your talks](#send-me-your-talks)
  - [Backstory](#backstory)
  - [License](#license)

## Features

* ✏️ Create **without effort** your presentation using HTML and CSS

* 📰 Use predefined **templates**

* 🌈 **Style** your presentation quickly

* 🌅 Create a **lightweight** presentation where images are **lazy** loaded

* 📱 Ship your presentation as a **Progressive Web App**

* 🎁 Free and **open source**

### Starter kit extra features

[DeckDeckGo] offers also a [starter kit](https://github.com/fluster/deckdeckgo-starter) kit which lets you additionally:

* 🚀 Publish your presentation as a **Progressive Web App** with a **offline** support

* 🦄 Use **Ionic** components and icons to create the content or even add extra features

### Remote control

Cherry on the cake 🍒🎂 [DeckDeckGo] comes with its [Progressive Web App](https://deckdeckgo.app) that allows you to remote control your presentation 🚀

## Getting Started

[DeckDeckGo] provides a [CLI](https://github.com/fluster/create-deckdeckgo) and a [starter kit](https://github.com/fluster/deckdeckgo-starter).

To get started and **to create your presentation**, run the following command and follow the prompt:

```bash
npm init deckdeckgo
```

Once your presentation created, go to your new project's folder and start editing `src/index.html` to prepare your slides and content for your talk 😉

## Installing DeckDeckGo in any projects

If you wish to use the [DeckDeckGo] presentation tool without any tooling, use it directly in your project from a CDN, using a simple script include, or install it from [npm](https://www.npmjs.com/package/deckdeckgo).

### Using DeckDeckGo from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script src="https://unpkg.com/deckdeckgo@latest/dist/deckdeckgo.js"></script>
```

### Install DeckDeckGo from NPM

Install [DeckDeckGo] in your project from [npm](https://www.npmjs.com/package/deckdeckgo) using the following command:

```bash
npm install deckdeckgo
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

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

The [detailed documentation](doc/slides/slides.md) display all templates and all options available for each slides.

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

## Talks

A collection of talks where [DeckDeckGo] was used:

| Title                      | Event   | Author and repo   | Available online          |
| -------------------------- |:-----------------:|:-----------------:| ---------------:|
| Ionic v4, web components, shadow dom and beyond | 2018/10/16 [Pantalks](https://www.meetup.com/fr-FR/Pantalks-tech-non-tech-talks-Panter-AG-Zurich/events/255430094/), Zürich | [Peterpeterparker](https://github.com/peterpeterparker/ionicv4-and-beyond) |  |
| Ionic v4 and web components | 2018/10/26 [Web Zürich October](https://www.meetup.com/fr-FR/Web-Zurich/events/255699446), Zürich | [Peterpeterparker](https://github.com/peterpeterparker/webzueri) |  |

### Send me your talks

If you would publish online a talk you would have built with [DeckDeckGo], reach me out, I would be super duper happy to list these ❤️

## Backstory

I had the opportunity to talk about Web Components and Ionic. While I was developing my presentation it came to my mind that I was not really following what I was about to present, that's why I wrapped up together [DeckDeckGo], this new tool to create lightweight presentation using HTML, Web Components and Ionic.

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com)

[DeckDeckGo]: https://deckdeckgo.com
