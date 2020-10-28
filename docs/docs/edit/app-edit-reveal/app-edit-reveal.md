# Reveal

Make elements and text appear one line at a time in [DeckDeckGo].

## Table of contents

- [Introduction](#app-edit-reveal-introduction)
- [Triggers](#app-edit-reveal-triggers)
- [Exception](#app-edit-reveal-exception)
- [Edit](#app-edit-reveal-edit)
  - [Examples](#app-edit-reveal-examples)
- [List](#app-edit-reveal-list)
  - [Examples for list](#app-edit-reveal-examples-for-list)
- [Installation](#app-components-social-installation)
  - [Using from a CDN](#app-components-social-install-from-a-cdn)
  - [Install from NPM](#app-components-social-install-from-npm)
  - [Framework integration](#app-components-social-framework-integration)

## Introduction

Per default all the content of each slide and component is visible. If you wish to make elements and text appear one line at a time, it's up to you using the following supported options.

## Triggers

The animation of such elements will happen when you or your user will use the navigation buttons on the keyboard or the navigation buttons in the [remote control](https://deckdeckgo.app).

## Exception

Elements set as "to be animated" are going to be displayed on mobile devices, that's a design choice, but if you wish to activate the animation for mobile too, you could set the deck's property `revealOnMobile` to `true`.

> I (David here) think that it is better in terms of mobile UX. For example, if a slide would contain for example 10 elements, the users would have to swipe the slide 10 times before being able to read the entire content and navigate. I'm open to suggestion and discussion about it, if you would rather like a different default behavior.

## Edit

[DeckDeckGo] provides a component `<deckgo-reveal/>` which should be used in case you would like to make elements appear one at a time. Simply put your element with your content inside, that's it.

Good to know, the component could be use as a child of a `slot` you would pass to a slide or could also be use as `slot` value, as you wish.

Nota bene, at least one element should be provided, adding only text inside the component would not work as the detection is based on elements.

### Examples

The component `deckgo-reveal` use as `slot`:

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <deckgo-reveal slot="content">
      <p>Hello World 🚀</p>
    </deckgo-reveal>
  </deckgo-slide-title>
</deckgo-deck>
```

Many components `deckgo-reveal` as children:

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <p slot="content">
      <deckgo-reveal><span>Hello World One 🚀</span></deckgo-reveal>
      <deckgo-reveal><span>Hello World Two 🚀</span></deckgo-reveal>
      <deckgo-reveal><span>Hello World Three 🚀</span></deckgo-reveal>
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

Or a component `deckgo-reveal` as child containing children:

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <div slot="content">
      <deckgo-reveal>
         <p>Hello World One 🚀</p>
         <p>Hello World Two 🚀</p>
         <p>Hello World Three 🚀</p>
      </deckgo-reveal>
    </div>
  </deckgo-slide-title>
</deckgo-deck>
```

## List

You could use the above component to encapsulate each `li` element of your lists, I guess that would work out, but [DeckDeckGo] also provides a dedicated component `<deckgo-reveal-list/>` to reveal list.

To use it, simply replace the opening tag of your list (`ul`, `ol` or `dl`) with it.

### Attributes

The following attributes could be applied to the element:

| Property  | Attribute  | Mandatory | Description                                   | Type     | Default |
| --------- | ---------- | --------- | --------------------------------------------- | -------- | ------- |
| `listTag` | `list-tag` |           | The type of list (`ol` default, `ul` or `dl`) | `string` | `ol`    |

## Theming

The following theming options are also available:

| CSS4 variable                    | Default                | Note                                                   |
| -------------------------------- | ---------------------- | ------------------------------------------------------ |
| --reveal-list-style              |                        | The list-style property of the list                    |
| --reveal-list-style-image        |                        | The list-style property of the list                    |
| --reveal-list-style-position     |                        | The list-style-position property of the list           |
| --reveal-list-margin             |                        | The list-margin property of the list                   |
| --reveal-list-padding            |                        | The list-padding property of the list                  |
| --reveal-list-background         |                        | The list-background property of the list               |
| --reveal-list-style-type         | `disc`                 | The list-style-type property in case of `ul` container |
| --reveal-list-style-type         | `decimal`              | The list-style-type property in case of `ol` container |
| --reveal-list-style-type         | `none`                 | The list-style-type property in case of `dl` container |
| --reveal-opacity-not-loaded      | `0`                    | If not displayed, the component is hidden              |
| --reveal-opacity-loaded          | `1`                    | The opacity if displayed                               |
| --reveal-list-opacity-not-loaded | `0`                    | If not displayed, the component is hidden              |
| --reveal-list-opacity-loaded     | `1`                    | The opacity if displayed                               |
| --reveal-transition              | `opacity 0.15s linear` | The animation of the component                         |
| --reveal-list-transition         | `opacity 0.15s linear` | The animation of the component list                    |
| --reveal-display                 | `block`                | The display property of the component                  |
| --reveal-list-display            | `opacity 0.15s linear` | The display property of the component list             |

## Examples for list

Likewise, the component could be used as a child of a `slot` you would pass to a slide or could also be use as `slot` value, as you wish.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <div slot="content">
      <deckgo-reveal-list list-tag="ul">
         <li>Hello World One 🚀</li>
         <li>Hello World Two 🚀</li>
         <li>Hello World Three 🚀</li>
      </deckgo-reveal-list>
    </div>
  </deckgo-slide-title>
</deckgo-deck>
```

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] reveal component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/reveal@latest/dist/deckdeckgo-reveal/deckdeckgo-reveal.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/reveal) using the following command:

```bash
npm install @deckdeckgo/reveal
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/reveal';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/reveal/dist/loader';
deckDeckGoElement();
```

[deckdeckgo]: https://deckdeckgo.com
