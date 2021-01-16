# Introduction

[DeckDeckGo] is a deck of slides. Each of these is based on a template which has its own layout and behaviour and not other limit than what the web can offer.

Their content could be edited and structured using `slots` and other attributes.

The deck itself is declared using the tag `<deckgo-deck/>` and each slides can be added as its children.

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

In the previous example, the presentation contains two slides. The first slide use the template `deckgo-slide-title` and the second slide use the template `deckgo-slide-content`.

# Installation

The core component of [DeckDeckGo](`<deckgo-deck/>`) does not contain any slides. These have to be explicitly installed and imported. Doing so, only these, which you are actually using, are going to be bundled in your presentations for the best performances.

> If you are using the Starter Kit, all default templates, these listed here behind, are pre-installed and pre-imported.

# Templates

[DeckDeckGo] provides a set of default templates. More templates are also available thanks to the wonderful work of the [community](https://github.com/deckgo-community).

## Default Templates

- Slide: [Title](/slides/title)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-title>
        <h1 slot="title">slot="title"</h1>
        <p slot="content">
          slot="content"
        </p>
      </deckgo-slide-title>
  </deckgo-deck>
</div>

- Slide: [Content](/slides/content)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-content>
      <h1 slot="title">slot="title"</h1>
      <p slot="content">
        slot="content"
      </p>
    </deckgo-slide-content>
  </deckgo-deck>
</div>

- Slide: [Split](/slides/split)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-split>
      <h1 slot="title">slot="title"</h1>
      <p slot="start">
        slot="start"
      </p>
      <p slot="end">
        slot="end"
      </p>
    </deckgo-slide-split>
  </deckgo-deck>
</div>

- Slide: [GIF](/slides/gif)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
      <h1 slot="title">slot="title"</h1>
      <h1 slot="header" style={{fontSize: 'var(--font-size-h1)'}}>slot="header"</h1>
      <h2 slot="footer" style={{fontSize: 'var(--font-size-normal)'}}>slot="footer"</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>

- Slide: [Chart](/slides/chart)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

- Slide: [YouTube](/slides/youtube)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>

- Slide: [Code](/slides/code)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-code src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/code/src/components/slide/deckdeckgo-slide-code.tsx">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-code>
  </deckgo-deck>
</div>

- Slide: [Author](/slides/author)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-author img-mode="circle" img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social twitter="daviddalbusco"><ion-icon aria-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon></deckgo-social></div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social linkedin="david-dal-busco"><ion-icon aria-label="David on LinkedIn" slot="icon" name="logo-linkedin"></ion-icon></deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>

- Slide: [QR Code](/slides/qrcode)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

- Slide: [Poll](/slides/poll)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-poll poll-link="https://deckdeckgo.com/poll" socket-url="https://api.deckdeckgo.com" connectPollSocket={false}>
        <h1 slot="question">Do you like my presentation so far?</h1>
        <p slot="answer-1">It is super</p>
        <p slot="answer-2">Meh</p>
        <p slot="answer-3">I could'nt care less</p>
        <p slot="answer-4">Tell me why</p>
        <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> and use the code {0}</p>
        <p slot="awaiting-votes">Awaiting first votes</p>
        <p slot="answer-5">Ain't nothin' but a heartache</p>
      </deckgo-slide-poll>
  </deckgo-deck>
</div>

- Slide: [Aspect Ratio](/slides/aspectratio)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-aspect-ratio grid={true}>
        <h1 style={{position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)', margin: '0'}}>Any elements</h1>
      </deckgo-slide-aspect-ratio>
  </deckgo-deck>
</div>

- Slide: [Playground](/slides/playground)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-playground src="https://codepen.io/peterpeterparker/pen/dyGbOZm">
      <h1 slot="title">Playground</h1>
    </deckgo-slide-playground>
  </deckgo-deck>
</div>

## Community

Developers and maintainers contributions are gathered in a [GitHub org](https://github.com/deckgo-community). Find there more templates and documentations about their usages.

[deckdeckgo]: https://deckdeckgo.com
