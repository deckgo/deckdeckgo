# Social

The "Social" component allows you to easily add a social link to your presentation.

## Table of contents

- [Showcase](#showcase)
- [Usage](#usage)
  - [Slots](#slots)
  - [Attributes](#attributes)
  - [Examples](#examples)

## Showcase

<p>
  <deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon> Twitter</deckgo-social>
</p>

<p>
  <deckgo-social github="fluster/deckdeckgo"><ion-icon slot="icon" name="logo-github"></ion-icon> DeckDeckGo on Github</deckgo-social>
</p>

## Usage

The "Social" slide's Web Component could be integrated using the tag `<deckgo-social/>`.

```
<deckgo-social twitter="daviddalbusco">
  <img data-src="/assets/twitter.svg" slot="icon"/>
  Twitter
</deckgo-social>
```

### Slots

The slot `icon` and the text are both optional. Of course, if you provide nothing, nothing will be rendered.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| twitter | string |  | Your Twitter username. It will be concatenated automatically with `https://twitter.com/` |
| linkedin | string |  | Your Linkedin username. It will be concatenated automatically with `https://www.linkedin.com/in/` |
| medium | string |  | Your Medium username. It will be concatenated automatically with `https://medium.com/@` |
| github | string |  | Your Github username. It will be concatenated automatically with `https://github.com/` |
| fullUrl | string |  | In case you would like to provide the URI of your choice |

### Examples

Without any icons:

```
<deckgo-social twitter="daviddalbusco">Twitter</deckgo-social>
<deckgo-social linkedin="david-dal-busco/">Linkedin</deckgo-social>
<deckgo-social medium="david.dalbusco">Medium</deckgo-social>
<deckgo-social full-url="https://stackoverflow.com/users/5404186/peter-parker">Stackoverflow</deckgo-social>
```

With for example `ion-icon`:

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
  Twitter
</deckgo-social>

<deckgo-social github="fluster/deckdeckgo">
  <ion-icon slot="icon" name="logo-github"></ion-icon>
  DeckDeckGo on Github
</deckgo-social>
```

[DeckDeckGo]: https://deckdeckgo.com 