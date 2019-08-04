# Slide: Countdown

The "Countdown" slide displays a countdown until your presentation starts.

It could be handy, for example when you organize a meetup, to display a countdown until the event start.

## Table of contents

- [Layout](#app-slide-countdown-layout)
- [Usage](#app-slide-countdown-usage)
  - [Usage](#app-slide-countdown-usage-1)
  - [Slots](#app-slide-countdown-slots)
  - [Notes](#app-slide-countdown-notes)
- [Attributes](#app-slide-countdown-attributes)
  - [Example without any slots](#app-slide-countdown-example-without-any-slots)
- [Theming](#app-slide-countdown-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-countdown hours="1" minutes="0" seconds="5">
        <h1 slot="title">slot="title"</h1>
        <p slot="hours">slot="hours"</p>
        <p slot="minutes">slot="minutes"</p>
        <p slot="seconds">slot="seconds"</p>
    </deckgo-slide-countdown>
  </deckgo-deck>
</div>

## Usage

The "Countdown" slide's Web Component could be integrated using the tag `<deckgo-slide-countdown/>`.

### Usage

```
<deckgo-deck>
    <deckgo-slide-countdown hours="1" minutes="0" seconds="5">
        <h1 slot="title">My presentation starts at</h1>
        <p slot="hours">Hours</p>
        <p slot="minutes">Minutes</p>
        <p slot="seconds">Seconds</p>
    </deckgo-slide-countdown>
</deckgo-deck>  
```

### Slots

The slots `title` as well as `hours`, `minutes` and `seconds` are optional.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Attributes

The time until your presentation should be provided to render the countdown. This amount of time should at least be given in seconds (max. 59). For that purpose, this components exposes the following attributes:
                                                                                                                                                                                                                                                        
| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| hours | number | 0 | The amount of hours before your presentations (max. 23) |
| minutes | number | 0 | The amount of minutes before your presentations (max. 59) |
| seconds | number | 0 | The amount of seconds before your presentations (max. 59) |
| custom-background | boolean | false | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions | boolean | false | If you would provide actions for the all deck and a specific one for this slide, set this option to `true` |

### Example without any slots

```
<deckgo-deck>
  <deckgo-slide-qrcode hours="0" minutes="10" seconds="45">
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
| --slide-countdown-container-padding-bottom | 64px | The bottom padding of the displayed time container |
| --slide-countdown-digits-width | 16em | The width of two displayed digit (Example: 16:00:00, 16em is the width of a digits 00 or 16) including spacing |
| --slide-countdown-digits-width  | 6em | The width of the (two) digits |
| --slide-countdown-digits-height |  | The height of the (two) digits |
| --slide-countdown-digits-background |  | The background color of the (two) digits |
| --slide-countdown-digits-border-radius |  | The border-radius of the (two) digits |
| --slide-countdown-digits-box-shadow | 0 3px 4px 0 rgba(0, 0, 0, .2), inset 2px 4px 0 0 rgba(255, 255, 255, .08) | The box-shadow of the (two) digits |
| --slide-countdown-digit-margin-right | 0.625em | The space between two digits |
| --slide-countdown-digits-font-size | 5em | The border-radius of the (two) digits |
| --slide-countdown-digits-font-weight |  | The font-weight of the (two) digits |
| --slide-countdown-digits-color |  | The color of the (two) digits |

[DeckDeckGo]: https://deckdeckgo.com