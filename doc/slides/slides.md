# DeckDeckGo - Slides

[DeckDeckGo] is a deck of slides where each slide has its own layout and behaviour. Their content could be edited and structured using the provided `slots`.

Some templates offer extra features as for example the slide [Code](#slide-code) which optionally offer the ability to zoom in the displayed code.

## Table of contents

- [Slides](#slides)
  - [Slide: Title](#slide-title)
  - [Slide: Content](#slide-content)
  - [Slide: Split](#slide-split)
  - [Slide: Gif](#slide-gif)
  - [Slide: Chart](#slide-chart)
  - [Slide: Youtube](#slide-youtube)
  - [Slide: Code](#slide-code)
  - [Slide: Author](#slide-author)
- [Background](#background)

## Slides

[DeckDeckGo] provide the following templates:

* [Title](#slide-title)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-title-layout.png" width="200px">

* [Content](#slide-content)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-content-layout.png" width="200px">

* [Split](#slide-split)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-split-layout.png" width="200px">

* [Gif](#slide-gif)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-gif-layout.gif" width="200px">

* [Chart](#slide-chart)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-chart-pie-layout.png" width="200px">
<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-chart-line-layout.png" width="200px">

* [Youtube](#slide-youtube)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-youtube-layout.png" width="200px">

* [Code](#slide-code)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-code-layout.gif" width="200px">

* [Author](#slide-author)

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-author-layout.png" width="200px">

*Note: If you would miss or need further templates, don't hesitate to open an issue and/or submit a PR, it would be my pleasure to add more options*

### Slide: Title

The "Title" slide is a simple slide which display its title and content center in the middle of the page.

This slide could be for example use for the very first and last slide of your presentation.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-title-layout.png" width="450px">

#### Usage

The "Title" slide's Web Component could be integrated using the tag `<deckgo-slide-title/>`.

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

##### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

##### Example

```
<deckgo-deck>
  <deckgo-slide-title reveal="true" reveal-show-first="true">
    <h1 slot="title">My presentation title</h1>
    <ul slot="content">
      <li>Hello</li>
      <li>World</li>
      <li>🚀</li>
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |

### Slide: Content

The "Content" slide is a simple slide which display its title and content aligned to the start of the page.

This slide could be for example use for the every slides of your presentation where you would like to display content related to your talk.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-content-layout.png" width="450px">

#### Usage

The "Content" slide's Web Component could be integrated using the tag `<deckgo-slide-content/>`.

```
<deckgo-deck>
  <deckgo-slide-content>
    <h1 slot="title">Something related to my topic</h1>
    <p slot="content">
      Cool beans
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

##### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

##### Example

```
<deckgo-deck>
  <deckgo-slide-content reveal="true" reveal-show-first="true">
    <h1 slot="title">Something related to my topic</h1>
    <ul slot="content">
      <li>Cool</li>
      <li>Beans</li>
    </p>
  </deckgo-slide-content>
</deckgo-deck>
```

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |

### Slide: Split

The "Split" slide is a simple slide which display two panes on the page.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-split-layout.png" width="450px">

#### Usage

The "Split" slide's Web Component could be integrated using the tag `<deckgo-slide-split/>`.

```
<deckgo-deck>
  <deckgo-slide-split>
    <h1 slot="title">Two columns subject</h1>
    <p slot="start">
      The content you want to display on the left side of the page
    </p>
    <p slot="end">
      The content you want to display on the right side of the page
    </p>
  </deckgo-slide-split>
</deckgo-deck>
```

##### Slots

Both slots `title`, `start` and `end` are optional. Without providing one of them, the page will remain empty.

The `start` slot is the content of the left pane respectively the slot `end` is the content of the right pane.

Note: The slot `title` is per default hidden even if you provide it. See attributes below if you wish to display it. 

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-split-background-start | | Left split pane background |
| --slide-split-color-start | | Left split pane color |
| --slide-split-background-end | | Right split pane background |
| --slide-split-color-end | | Right split pane color |
| --slide-split-padding-top | 16px | Padding top of a slide split pane |
| --slide-split-padding-end | 32px | Padding right of a slide split pane |
| --slide-split-padding-bottom | 16px | Padding bottom of a slide split pane |
| --slide-split-padding-start | 32px | Padding left of a slide split pane |
| --slide-split-title-padding-top | 16px | Padding top of the title of the |
| --slide-split-title-padding-end | 32px | Padding right of the title of the |
| --slide-split-title-padding-bottom | 16px | Padding bottom of the title of the |
| --slide-split-title-padding-start | 32px | Padding left of the title of the |
| --slide-padding-start | 32px | Modify slotted ul and ol padding-inline-start |
| --slide-split-align | inherit | Modify for example to center if you want to align the content in the middle |
| --slide-split-text-align | inherit | Modify for example to center if you want to align the text in the middle |
| --slide-split-title-display | none | The `slot` title is per default hidden even if you provide it. If you wish to displays it, modify this attribute |
| --zIndex | 1 | The z-index of the slide |

### Slide: Gif

The "Gif" slide let you add easily a gif, like those provided by [Giphy](https://giphy.com), to your presentation.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-gif-layout.gif" width="450px">

#### Note

The slide Gif is useful for Gifs but could be use for any images too, in case you would like for example to display an image fullscreen

#### Usage

The "Gif" slide's Web Component could be integrated using the tag `<deckgo-slide-gif/>`.

```
<deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="title">My title</h1>
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

#### Slots

The slots `title`, `header` and `footer` are both optional. `header` and `footer` would be displayed over the gif.

#### Gif component

The slide "Gif" relies on the component `<deckgo-gif/>` which is described in the components [documentation](https://github.com/fluster/deckdeckgo/blob/master/doc/components/components.md).

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src | string |  | The source url, the src, of the Gif. Could be an embeddable external url or a local one. |
| alt | string |  | And alt information could be provided for accessibility reason. |
| fullscreen | number | false | If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen. |

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |

### Slide: Chart

The "Chart" slide let you draw easily charts in your presentation.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-chart-pie-layout.png" width="450px">
<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-chart-line-layout.png" width="450px">

#### Usage

The "Chart" slide's Web Component could be integrated using the tag `<deckgo-slide-chart/>`.

```
<deckgo-slide-chart src="./assets/csv/data-pie-chart.csv">
    <h1 slot="title">My Pie chart</h1>
</deckgo-slide-chart>
```

##### Slots

The slot `title` is optional.

##### Chart components

The slide "Chart" relies on the charts components `<deckgo-pie-chart/>` and `<deckgo-line-chart/>` which are described in the components [documentation](https://github.com/fluster/deckdeckgo/blob/master/doc/components/components.md).

##### Installation

**Important note**: The [DeckDeckGo] charts components are provided in separate extra library. If you wish to add charts to your presentation, you will need to install this extra component from a CDN or [npm](https://www.npmjs.com/package/deckdeckgo). See the related documentation from its [installation guide](https://github.com/fluster/deckdeckgo-charts#getting-started).

From a CDN:

```
<script src="https://unpkg.com/deckdeckgo-charts@latest/dist/deckdeckgo-charts.js"></script>
```

From NPM:

```
npm install deckdeckgo-charts
```

The reasons behind are the following:

1. If you don't use charts, you will not have to install unnecessary dependencies
2. Cherry on the cake, the [DeckDeckGo] charts Web Component could be use in [DeckDeckGo] as in any other web projects

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| type | string | pie | The type of the chart, `pie` or `line` |

Furthermore, this slide component offers the same attributes as the [DeckDeckGo] charts Web Component, see its [documentation](https://github.com/fluster/deckdeckgo-charts) for the details.

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |
| --slide-chart-margin-top | 32px | Margin top of the chart inside its container |
| --slide-chart-margin-end | 96px | Margin right of the chart inside its container |
| --slide-chart-margin-bottom | 32px | Margin bottom of the chart inside its container |
| --slide-chart-margin-start | 32px | Margin left of the chart inside its container |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] charts Web Component, see its [documentation](https://github.com/fluster/deckdeckgo-charts) for the details.

### Slide: Youtube

The "Youtube" slide let you add easily a [Youtube](https://youtube.com) video to your presentation.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-youtube-layout.png" width="450px">

#### Usage

The "Youtube" slide's Web Component could be integrated using the tag `<deckgo-slide-youtube/>`.

```
<deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
  <h1 slot="title">A 16/9 video</h1>
</deckgo-slide-youtube>
```

##### Slots

The slot `title` is optional.

##### Youtube component

The slide "Youtube" relies on the component `<deckgo-youtube/>` which is described in the components [documentation](https://github.com/fluster/deckdeckgo/blob/master/doc/components/components.md).

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src | string |  | The source url, the Youtube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by Youtube. |
| width | number | Per default the video width will be calculated according the content size available. | Using this option you would be able to define your own width. |
| height | number | Per default the video height will be calculated according the content size available. | Using this option you would be able to define your own height. |

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |

#### Methods

The slide "Youtube" offers extra methods to play and pause the Youtube video clip. These methods are notably used by the [DeckDecGo]'s remote control.

##### Play the video

```
const slide = deck.getElementsByTagName('deckgo-slide-youtube');
await slide.play();
```

##### Pause the video

```
const slide = deck.getElementsByTagName('deckgo-slide-youtube');
await slide.pause();
```

### Slide: Code

The "Code" slide is a the slide to use if you would like to showcase code during your talk.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-code-layout.gif" width="450px">

#### Usage

The "Code" slide's Web Component could be integrated using the tag `<deckgo-slide-code/>`.

You could provide a file URI to the code you want to display or provide it with a slotted element.

##### Usage with file URI

```
<deckgo-deck>
  <deckgo-slide-code src="https://domain.com/path-to-my-code.extension">
    <h1 slot="title">My code</h1>
  </deckgo-slide-code>
</deckgo-deck>  
```

##### Usage with slotted element

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

##### Slots

The slots `title` and `code` are optional.

#### Attributes

The attribute `src` is for this component mandatory. Other attributes are optional.

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src | string | | The web url to the source code you would like to showcase |
| anchor | string | // DeckDeckGo | The anchor identifier which will be use to scroll through your code when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| anchor-zoom | string | // DeckDeckGoZoom | The anchor identifier which will be use to zoom inside your code when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| hide-anchor | boolean | true | Set this attribute to `false` in case you would like to actually display the anchor value too |
| language | string | javascript | Define the language to be used for the syntax highlighting. The list of [supported languages](https://prismjs.com/#languages-list) is defined by [Prism.js](https://prismjs.com/#languages-list) |

##### Example with file URI

```
<deckgo-deck>
  <deckgo-slide-code hide-anchor="fals" src="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
    <h1 slot="title">Code</h1>
  </deckgo-slide-code>
</deckgo-deck>
```

##### Example with slotted element

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

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --code-color |  | The color of the displayed code |
| --code-font-size |  | The size of the font for the code |
| --code-font-family |  | The family of the font for the code |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --code-margin-bottom | 64px | Padding bottom of the code scroller |
| --zoom-code | 1 | If you wish to manually zoom the code |
| --zIndex | 1 | The z-index of the slide |

#### Syntax highlighting

The library [Prism.js](https://prismjs.com) from [James DiGioia](https://github.com/mAAdhaTTah) is use for the syntax highlighting.

##### Note regarding highlight extra languages

No extra [Prism.js](https://prismjs.com) languages than the default one are bundled in the [DeckDeckGo] Web Component. If you would specify another `language` (see above list of attributes) the component will inject and load the required extra scripts from [unpkg](https://unpkg.com) at runtime. 

### Slide: Author

The "Author" slide let you introduce the author of the presentation.

#### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-author-layout.png" width="450px">

#### Usage

The "Author" slide's Web Component could be integrated using the tag `<deckgo-slide-author/>`.

```
<deckgo-deck>
  <deckgo-slide-author img-src="/assets/author.jpeg" img-alt="My self">
    <h1 slot="title">Author</h1>
    <div slot="author">
      <h2>David</h2>
      <p>Something about me</p>
    </div>
    <div slot="social-link"><deckgo-social twitter="daviddalbusco">twitter</deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>  
```

##### Slots

Both slots `title`, `author` and `social-link` are optional, but of course the slide would looks better with at least the slot `author` would be provided.

Notes: 

* The slot `title` is hidden. If you use the [DeckDeckGo] starter, it will be use for the navigation modal

* You could provide up to six `social-link` slots. Each of these could be your custom code or you could use the component `<deckgo-social/>` to easily provide a link to an external URI.

##### Social components

The details of the component `<deckgo-social/>` is described in the components [documentation](https://github.com/fluster/deckdeckgo/blob/master/doc/components/components.md).

#### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| img-src | string |  | An image URI, for example a picture of the author. Note: this image will be displayed as a circle. |
| img-alt | string |  | An optional accessibility alt for the image. |

##### Example

```
<deckgo-deck>
  <deckgo-slide-author img-src="/assets/author.jpeg">
    <div slot="author">
      <h2>David</h2>
      <p>Something about me</p>
    </div>
    <div slot="social-link"><deckgo-social twitter="daviddalbusco">twitter</deckgo-social></div>
    <div slot="social-link"><deckgo-social linkedin="david-dal-busco/">linkedin</deckgo-social></div>
    <div slot="social-link"><deckgo-social medium="david.dalbusco">medium</deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>
```

#### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-author-background-start | | Left pane background |
| --slide-author-color-start | | Left pane color |
| --slide-author-background-end | | Right pane background |
| --slide-author-color-end | | Right pane color |
| --slide-author-padding-top | 16px | Padding top of a slide |
| --slide-author-padding-end | 32px | Padding right of a slide |
| --slide-author-padding-bottom | 16px | Padding bottom of a slide |
| --slide-author-padding-start | 32px | Padding left of a slide |
| --slide-padding-start | 32px | Modify slotted ul and ol padding-inline-start |
| --slide-author-align | inherit | Modify for example to center if you want to align the content in the middle |
| --slide-author-text-align | inherit | Modify for example to center if you want to align the text in the middle |
| --slide-author-img-size | 80% | The size of the image of the left pane |
| --slide-author-social-padding-top | 32px | The spacing between the author description and the social links |
| --zIndex | 1 | The z-index of the slide |

## Background

Beside slides and templates, a [DeckDeckGo] deck could also contains a customized element `background` which could be injected using a dedicated `slot`. This element inherit the length of the all presentation. This option is handy for example in case you would like to have a background which follows your presentation.

### Example

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <p slot="content">
      Hello World 🚀
    </p>
  </deckgo-slide-title>
  
  <div class="circle" slot="background"></div>
</deckgo-deck>
```

where for example the related `circle` css code could be:

```
div.circle {
  position: absolute;
  left: 50%;
  transform: translate(-50%, 0);

  bottom: -10vh;
  width: 800vw;
  height: 100vh;

  border-radius: 50%;
  background: yellow;
  opacity: 0.3;
}
```

[DeckDeckGo]: https://deckdeckgo.com