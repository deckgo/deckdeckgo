# Concept

[DeckDeckGo] is a deck of slides where each slide is based on a template which has its own layout and behaviour. Their content could be edited and structured using `slots` and other attributes.

The parent deck should be declared using the tag `<deckgo-deck/>` and each slide should be added as its children.

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

The core component of [DeckDeckGo] (`<deckgo-deck/>`) does not contain any slides, these have to be explicitly installed and imported. Doing so, only these, which you are actually using, are going to be bundled in your presentations for the best performances.

> If you are using the Starter Kit, per default, all our templates, these listed here behind, are pre-installed and pre-imported.

# Templates

[DeckDeckGo] provide the following templates:

* Slide: [Title](/slides/title)

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

* Slide: [Content](/slides/content)

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

* Slide: [Split](/slides/split)

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

* Slide: [Gif](/slides/gif)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
      <h1 slot="title">slot="title"</h1>
      <h1 slot="header" style={{fontSize: 'var(--font-size-h1)'}}>slot="header"</h1>
      <h2 slot="footer" style={{fontSize: 'var(--font-size-normal)'}}>slot="footer"</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>

* Slide: [Chart](/slides/chart)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/fluster/deckdeckgo-charts/master/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

* Slide: [Youtube](/slides/youtube)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>

* Slide: [Code](/slides/code)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-code src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/core/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-code>
  </deckgo-deck>
</div>

* Slide: [Author](/slides/author)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-author img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social twitter="daviddalbusco"><ion-icon area-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon> twitter</deckgo-social></div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social linkedin="david-dal-busco/"><ion-icon area-label="David on Linkedin" slot="icon" name="logo-linkedin"></ion-icon> linkedin</deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>

* Slide: [QR Code](/slides/qrcode)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

* Slide: [Countdown](/slides/countdown)

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-countdown hours={1} minutes={0} seconds={5}>
        <h1 slot="title">slot="title"</h1>
        <p slot="hours">slot="hours"</p>
        <p slot="minutes">slot="minutes"</p>
        <p slot="seconds">slot="seconds"</p>
    </deckgo-slide-countdown>
  </deckgo-deck>
</div>

[DeckDeckGo]: https://deckdeckgo.com 