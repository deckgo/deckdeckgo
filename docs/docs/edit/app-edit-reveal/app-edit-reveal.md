# Reveal

Make elements and texts appear one line at a time in [DeckDeckGo].

## Table of contents

- [Introduction](#app-edit-reveal-introduction)
- [Triggers](#app-edit-reveal-triggers)
- [Exception](#app-edit-reveal-exception)
- [Edit](#app-edit-reveal-edit)
    - [Examples](#app-edit-reveal-examples)
- [List](#app-edit-reveal-list)
    - [Examples for list](#app-edit-reveal-examples-for-list)

## Introduction

Per default all the content of each slides and components is visible. If you wish to make elements and texts appear one line at a time, it's up to you using the following supported options.

## Triggers

The animation of such elements will happen when you or your users will use the keyboard, the navigation buttons or the navigation buttons in the [remote control](https://deckdeckgo.app).

## Exception

Elements set as "to be animated" are going to be displayed in any case on mobile devices, that's a design choice.

> I (David here) think that it is better in terms of mobile UX. For example, if a slides would contains for example 10 elements, the users would have to swipe the slide 10 times before being able to read the all content and navigate. I'm open to suggestion and discussion about it, ping me, open a feature request or even submit a PR if you see this differently.   

## Edit

[DeckDeckGo] provide a component `<deckgo-reveal/>` which should be use in case you would like to make elements appear one at a time. Simply put your element with your content inside, that's it.

Good to know, the component could be use as a child of a `slot` you would pass to a slide or could also be use as `slot` value, as you wish.

Notabene, at least an element should be provided, adding only text inside the component would not work as the detection is based on elements. 

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

You could use the above component to encapsulate each `li` elements of your lists, I guess that would work out, but [DeckDeckGo] also provide a dedicated component `<deckgo-reveal-list/>` to reveal list.

To use it, simply replace the opening tag of your list (`ul`, `ol` or `dl`) with it.

### Attributes

The following attribute could be applied to the element:

| Property       | Attribute       | Mandatory | Description | Type      | Default                             |
| -------------- | --------------- | --------- | ----------- | --------- | ----------------------------------- |
| `listTag`         | `list-tag`          |  | The type of list (`ol` default, `ul` or `dl`) | `string`                | `ol` |

## Theming

The following theming options are also available:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --reveal-list-style | | The list-style property of the list |
| --reveal-list-style-image | | The list-style property of the list |
| --reveal-list-style-position | | The list-style-position property of the list |
| --reveal-list-margin | | The list-margin property of the list |
| --reveal-list-padding | | The list-padding property of the list |
| --reveal-list-background | | The list-background property of the list |
| --reveal-list-style-type | `disc` | The list-style-type property in case of `ul` container |
| --reveal-list-style-type | `decimal` | The list-style-type property in case of `ol` container |
| --reveal-list-style-type | `none` | The list-style-type property in case of `dl` container |

## Examples for list

Likewise, the component could be use as a child of a `slot` you would pass to a slide or could also be use as `slot` value, as you wish.

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

[DeckDeckGo]: https://deckdeckgo.com