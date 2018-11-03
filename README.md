# DeckDeckGo

Add a lightweight presentation to your web project using HTML and Web Components.

[DeckDeckGo] is build with [Stencil](https://stenciljs.com) and could be use in any modern framework or even without any.

## Table of contents

- [DeckDeckGo](#deckdeckgo)
	- [Table of contents](#table-of-contents)
	- [Features](#features)
		- [Starter kit extra features](#starter-kit-extra-features)
	- [Getting Started](#getting-started)
		- [Using DeckDeckGo from a CDN](#using-deckdeckgo-from-a-cdn)
		- [Install DeckDeckGo from npm](#install-deckdeckgo-from-npm)
		- [Framework integration](#framework-integration)
	- [Editing](#editing)
		- [Showcasing code](#showcasing-code)
	- [Navigation](#navigation)
	- [Extra features](#extra-features)
	- [Lazy loading](#lazy-loading)
	- [Theming](#theming)
	- [Talks](#talks)
		- [Send me your talks](#send-me-your-talks)
	- [Backstory](#backstory)
	- [License](#license)

## Features

* ✏️ Use HTML and CSS to create **without effort** your presentation

* 📰 Use predefined **templates**

* 🌈 **Style** your presentation quickly

* 🌅 Create a **lightweight** presentation where images are **lazy** loaded

* 📱 Ceate slides which looks good on **mobile** devices too

* 🎁 Free and **open source**

### Starter kit extra features

[DeckDeckGo] offers also a [starter kit](https://github.com/fluster/deckdeckgo-starter) kit which lets you additionally:

* 🚀 Bundles your presentation as a **Progressive Web App**

* 🦄 Use **Ionic** components and icons to create the content or even add extra features

*Note: [DeckDeckGo] will soon offer a CLI to let you initialize your presentation based on the starter kit more easily* 

## Getting Started

Use [DeckDeckGo] directly in your project from a CDN using a simple script include or install it from [npm](https://www.npmjs.com/package/deckdeckgo)

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

[DeckDeckGo] is a deck of slides where each slide has its own layout and behaviour. Their content could be edited and structured using the provided `slots`.

The slides [documentation](doc/slides/slides.md) display all templates and all options available for each slide.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <p slot="content">
      Hello World 🚀
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

### Showcasing code

[DeckDeckGo] offers an handy slide to showcase code in your presentation. Instead of having to copy/paste your code inside the presentation itself, the provided template only need an url to the piece of code you would like display.

For example, in the following example, the slide is showcasing a piece of code of [DeckDeckGo] hosted on [Github](https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx).

```
<deckgo-deck>
  <deckgo-slide-code src-file="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
    <h1 slot="title">My code</h1>
  </deckgo-slide-title>
</deckgo-deck>
```

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

Theming is an ongoing improvement I'm currently working on. The already available options are described in the slides [documentation](doc/slides/slides.md). 

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
