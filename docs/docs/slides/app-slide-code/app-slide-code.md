# Slide: Code

The "Code" slide is a the slide to use if you would like to showcase code during your talk.

## Table of contents

- [Layout](#app-slide-code-layout)
- [Usage](#app-slide-code-usage)
  - [Usage with file URI](#app-slide-code-usage-with-file-uri)
  - [Usage with slotted element](#app-slide-code-usage-with-slotted-element)
  - [Slots](#app-slide-code-slots)
  - [Notes](#app-slide-code-notes)
- [Code components](#app-slide-code-code-components)
- [Installation](#app-slide-code-installation)
- [Attributes](#app-slide-code-attributes)
  - [Example with file URI](#app-slide-code-example-with-file-uri)
  - [Example with slotted element](#app-slide-code-example-with-slotted-element)
- [Theming](#app-slide-code-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-code src="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
          <h1 slot="title">slot="title"</h1>
        </deckgo-slide-code>
  </deckgo-deck>
</div>

## Usage

The "Code" slide's Web Component could be integrated using the tag `<deckgo-slide-code/>`.

You could provide a file URI to the code you want to display or provide it with a slotted element.

### Usage with file URI

```
<deckgo-deck>
  <deckgo-slide-code src="https://domain.com/path-to-my-code.extension">
    <h1 slot="title">My code</h1>
  </deckgo-slide-code>
</deckgo-deck>  
```

### Usage with slotted element

```
<deckgo-deck>
  <deckgo-slide-code language="java">
      <h1 slot="title">Manual code</h1>
      <code slot="code">
  interface DeckDeckGoDemo {
    boolean helloWorld();
  }
  </deckgo-slide-code>
</deckgo-deck>  
```

### Slots

The slots `title` and `code` are optional.

This template also exposes a slot `info` which would let you display an information over your code on mobile devices, useful to explain your reader that they should click to switch between vertical and horizontal scrolling. Find an example of the use of that slot on the [DeckDeckGo] website.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Code components

The slide "Code" relies on the code component `<deckgo-highlight-code/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Installation

The [DeckDeckGo] - Hightlight code component is provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] code to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/deckdeckgo-highlight-code) as described in its [installation guide](https://github.com/deckgo/deckdeckgo-highlight-code#getting-started).

## Attributes

At least `src` or the `slot` code should be provided in order to render code in this template. It offers the same attributes as the [DeckDeckGo] code Web Component, see its [documentation](https://github.com/deckgo/deckdeckgo-highlight-code) for the details.

### Example with file URI

```
<deckgo-deck>
  <deckgo-slide-code hide-anchor="false" src="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
    <h1 slot="title">Code</h1>
  </deckgo-slide-code>
</deckgo-deck>
```

### Example with slotted element

```
<deckgo-deck>
  <deckgo-slide-code language="java">
      <h1 slot="title">Manual code</h1>
      <code slot="code">interface NumericTest {
    boolean computeTest(int n);
  }
  
  public static void main(String args[]) {
    NumericTest isEven = (n) -> (n % 2) == 0;
    NumericTest isNegative = (n) -> (n < 0);
  
    // Output: false
    System.out.println(isEven.computeTest(5));
  
    // Output: true
    System.out.println(isNegative.computeTest(-5));
  }</code>
  </deckgo-slide-code>
</deckgo-deck>  
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] - Highlight code Web Component, see its [documentation](https://github.com/deckgo/deckdeckgo-highlight-code) for the details.

[DeckDeckGo]: https://deckdeckgo.com